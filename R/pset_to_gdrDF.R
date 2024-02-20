#' Convert a PharmacoSet to a data.table that is prepare for input into gDR pipeline
#'
#' @param pharmacoset PharmacoSet object
#' @param run_parallel logical, TRUE (default) if to run functions in Parallel, FALSE to run in serial
#' @param workers integer, number of workers defaults to 2L if run_parallel is TRUE
#' @return data.table of PharmacoSet's dose response data with column names aligned with gDR standard
#' 
#' @examples
#' pset <- suppressMessages(getPSet(
#'   "Tavor_2020", 
#'   psetDir = system.file("extdata/pset", package = "gDRimport"),
#'   use_local_PSets_list = TRUE
#' ))
#' dt <- convert_pset_to_df(pset)
#' gDRutils::reset_env_identifiers()
#' 
#' @author Jermiah Joseph -- collaboration with BHKLab
#' @export
#'
convert_pset_to_df <- function(pharmacoset,
                           run_parallel = TRUE,
                           workers = 2L) {
  assertthat::assert_that(is.logical(run_parallel),
                          msg = "run_parallel must be a logical.")
  assertthat::assert_that(inherits(pharmacoset, "PharmacoSet"),
                          msg = "pharmacoset paramater must inherit from PharmacoSet class.")
  assertthat::assert_that(is.integer(workers),
                          msg = "workers parameter must be an integer. Default = 2L")
  
  # FUNCTION TO MANIPULATE ENV IDENTIFIERS
  setEnvForPSet()
  
  # GET DOSE AND VIABILITY DATA & MELT INTO LARGE TABLE
  dose_response <- .extractDoseResponse(pset = pharmacoset)
  
  # ADD IN DURATION AND REFERENCE DIVISION TIME
  dose_response_duration_refdivtime <- .createPseudoData(dose_response)
  
  # REMOVE NEGATIVE VIABILITIES
  .removeNegatives(dose_response_duration_refdivtime)
}

#' Adjust environment variables to meet gDR standards
#' 
#' @examples
#' setEnvForPSet()
#' gDRutils::reset_env_identifiers()
#' 
#' @return \code{NULL}
#' @export
setEnvForPSet <- function() {
  ## -- Set environment identifiers to map from our columns to gDR columns
  gDRutils::reset_env_identifiers()
  
  pgx_to_gdr_ids <- list(
      cellline = "Clid",
      cellline_name = "Clid",
      drug = "DrugName",
      drug_name = "DrugName",
      concentration = "Concentration",
      duration = "Duration",
      barcode = "Barcode"
  )
  
  invisible(Map(function(x, y) gDRutils::set_env_identifier(x, y),
                names(pgx_to_gdr_ids), pgx_to_gdr_ids))
}

#' Get PharmacoSet 
#' 
#' @param pset_name string with the name of the PharmacoSet
#' @param psetDir string with the temporary directory for the PharmacoSet
#' @param canonical logical flag indicating if the PSet canonical
#' @param timeout maximum number of seconds allowed for PSet download
#' @param use_local_PSets_list logical flag if PSets list should be used from 
#' local. If FALSE PSets list will be taken from web.
#' 
#' @examples  
#' suppressMessages(getPSet(
#'   "Tavor_2020", 
#'   psetDir = system.file("extdata/pset", package = "gDRimport"),
#'   use_local_PSets_list = TRUE
#' ))
#' 
#' @return PharmacoSet object
#' @export
getPSet <- function(pset_name,
                    psetDir = getwd(),
                    canonical = FALSE,
                    timeout = 600,
                    use_local_PSets_list = FALSE) {

  assertthat::assert_that(is.character(pset_name),
                          msg = "pset_name parameter must be a character vector.")
  
  checkmate::assert_character(getwd())
  checkmate::assert_flag(canonical)
  checkmate::assert_numeric(timeout)
  
  availPSets <- if (use_local_PSets_list) {
    qs::qread(system.file("extdata", "data_for_unittests", "PSets.qs", package = "gDRimport"))
  } else {
    PharmacoGx::availablePSets(canonical = canonical)
  }  
  
  pset_name_param <- if (pset_name %in% availPSets$"Dataset Name") {
    availPSets[availPSets$"Dataset Name" == pset_name, "PSet Name"]
    } else if (pset_name %in% availPSets$"PSet Name") {
      pset_name
      } else {
        stop(pset_name,
             " does not exist in the available PSets. Try one of the following:\n",
             paste(availPSets$`PSet Name`, collapse = "\n"))
        }
  
  # Check if PSet exists in directories where PSets are stored. 
  # Read in if exists, download otherwise
  pset <- if (file.exists(file.path(psetDir, paste0(pset_name_param, ".qs")))) {
    message("PSet exists in user-provided directory, reading .qs file")
    qs::qread(file.path(psetDir, paste0(pset_name_param, ".qs")))
    } else {
      message("PSet does not exist in user-provided directory, downloading from database.")
      PharmacoGx::downloadPSet(pset_name_param, saveDir = psetDir, timeout = timeout)
    }
  # Update PSet to new structure if not updated already.
  PharmacoGx::updateObject(pset)
}

#' Get dose and viability readouts and melt into large data table
#' @keywords internal
#' 
#' @return data.table with dose-reponse data
#' 
.extractDoseResponse <- function(pset) {
  checkmate::assert_class(pset, "PharmacoSet")
  
  tre <- pset@treatmentResponse
  raw_tr <- tre$raw
  info_dt <- data.table::as.data.table(tre$info, keep.rownames = TRUE)
  duration <- unique(tre$info$duration.hours)
  
  # use output of get_env_identifiers() 
  env_ids <- gDRutils::get_env_identifiers()
  # Determine how many doses there are
  raw_tr_dims <- dim(raw_tr)
  if (raw_tr_dims[3] < 2) {
      stop("PharmacoSet treatmentResponse raw data does not have
           either/both of viability and concentration data.")
  } else {
      viability <- data.table::as.data.table(raw_tr[, seq_len(raw_tr_dims[2]), 2], TRUE)
      viability[[env_ids$untreated_tag[1]]] <- 100
      viability.m <- data.table::melt(viability, 
                  measure.vars = c(2:length(viability)),
                  variable.name = "Dose",
                  value.name = "ReadoutValue")
  
      doses <- data.table::as.data.table(raw_tr[, seq_len(raw_tr_dims[2]), 1], TRUE)
      doses[[env_ids$untreated_tag[1]]] <- 0
      doses.m <- data.table::melt(doses, 
                  measure.vars = c(2:length(doses)),
                  variable.name = "Dose",
                  value.name = env_ids$concentration)
  }
  # CHECK IF SAME SIZE and MERGE     
  if (length(doses.m) == length(viability.m)) {
      merged_dt <- viability.m[doses.m, on = intersect(names(viability.m),
                                                       names(doses.m))]
  } else {
      sprintf("doses and viability data tables are not the same size")
  }
  treatment_cols <- c("sampleid", "treatmentid")
  selected_cols <- c(treatment_cols, "rn")
  merged_dt <- merged_dt[info_dt[, selected_cols, with = FALSE], on = "rn"]
  data.table::setnames(merged_dt, treatment_cols, c(env_ids$cellline, env_ids$drug_name))
  merged_dt[Dose == env_ids$untreated_tag[1], env_ids$drug_name := env_ids$untreated_tag[1]]
  merged_dt[, Dose := NULL]
  
  if (!is.null(duration)) {
    merged_dt[, (env_ids$duration) := duration]
  }
  merged_dt
}

#' Add in pseudo-data for duration and cell reference division time
#' @keywords internal
#' 
#' @return data.table
.createPseudoData <- function(dt) {
  
  checkmate::assert_data_table(dt)

  barcode <- gDRutils::get_env_identifiers("barcode")[1]
  duration <- gDRutils::get_env_identifiers("duration")
  refDivTime <- gDRutils::get_env_identifiers("cellline_ref_div_time")
  
  
  # Some datasets do not have duration specified so let's assign any value, since it is required by gDR
  if (!duration %in% names(dt)) {
    dt[, (duration) := 72]
  }
  if (!refDivTime %in% names(dt)) {
    dt[, (refDivTime) := NA]
  }
  if (!barcode %in% names(dt)) {
    colnames(dt)[1] <- barcode
  }
  
  dt
}


#' Remove negative viabilities
#' @keywords internal
#' 
#' @return data.table with positive values in column `ReadoutValue`
#' 
.removeNegatives <- function(dataset) {
  checkmate::assert_data_table(dataset)
  dataset[dataset$ReadoutValue > 0]
}
