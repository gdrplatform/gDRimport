## 
# collaboration with BHKLab
# co-author: Jermiah Joseph

## TO:DO Get PSET check if it exists in psetDir first!

# Goal: Single function in gDRimport (helper functions included) that
# converts a created PharmacoGx::PharmacoSet into a dataframe 
# that can be used as input for the gDR data processing pipeline.
# Aim 1: convert dose-response data from a PharmacoSet into dataframe 
#        with appropriate column names
# Aim 2: set environment identifiers accordingly.
#' Convert a PharmacoSet to a DataFrame that is prepare for input into gDR pipeline
#'
#' @param pharmacoset PharmacoSet object
#' @param run_parallel logical, TRUE (default) if to run functions in Parallel, FALSE to run in serial
#' @param workers integer, number of workers defaults to 2L if run_parallel is TRUE
#' @return data.frame of PharmacoSet's dose response data with column names aligned with gDR standard
#' @export
#'
convert_pset_to_df <- function(pharmacoset,
                                run_parallel = TRUE,
                                workers = 2L) {
    
    # TO:DO
    # ASSERT ALL PARAMETERS 
    assertthat::assert_that(is.logical(run_parallel),
                            msg = "run_parallel must be a logical.")
    assertthat::assert_that(inherits(pharmacoset, "PharmacoSet"),
                            msg = "pharmacoset paramater must inherit from PharmacoSet class.")
    assertthat::assert_that(is.integer(workers),
                            msg = "workers parameter must be an integer. Default = 2L")

    # FUNCTION TO MANIPULATE ENV IDENTIFIERS
    gDRimport::setEnvForPSet()

    # SET PARALLEL OR SERIAL
    .setParallel(runParallel = run_parallel, workers = workers)

    # GET DOSE AND VIABILITY DATA & MELT INTO LARGE TABLE
    dose_response <- .extractDoseResponse(PSet = pharmacoset)  

    # ADD IN DURATION AND REFERENCE DIVISION TIME
    dose_response_duration_refdivtime <- .createPseudoData(dose_response)

    # REMOVE NEGATIVE VIABILITIES
    dt <- .removeNegatives(dose_response_duration_refdivtime)

    as.data.frame(dt)
}

#' Adjust environment variables to meet gDR standards
#' 
#' @export
setEnvForPSet <- function() {
    ## -- Set environment identifiers to map from our columns to gDR columns
    gDRutils::reset_env_identifiers()
    (default_ids <- gDRutils::get_env_identifiers())

    pgx_to_gdr_ids <- list(
        cellline = "Clid",
        cellline_name = "Clid",
        drug = "DrugName",
        drug_name = "DrugName",
        concentration = "Concentration",
        duration = "Duration",
        barcode = "Barcode"
    )
   
    unmodified_ids <- default_ids[setdiff(names(default_ids), names(pgx_to_gdr_ids))]
    pgx_to_gdr_ids <- c(pgx_to_gdr_ids, unmodified_ids)
    ## FIXME:: Setting identifiers drops defaults?
    for (i in seq_along(pgx_to_gdr_ids)) {
        gDRutils::set_env_identifier(
            k = names(pgx_to_gdr_ids)[i],
            v = pgx_to_gdr_ids[[i]]
        )
    }
}

#' Get PharmacoSet 
#' @keywords internal
.getPSet <- function(pset_name,
                    psetDir = getwd(),
                    canonical = FALSE,
                    timeout = 600) {
   
    assertthat::assert_that(is.character(pset_name),
                            msg = "pset_name parameter must be a character vector.")

    availPSets <- PharmacoGx::availablePSets(canonical = canonical)

    if (pset_name %in% availPSets$"Dataset Name") {
        pset_name_param = availPSets[availPSets$"Dataset Name"== pset_name,]$"PSet Name"
    } else if (pset_name %in% availPSets$"PSet Name") {
        pset_name_param = pset_name    
    } else {
        stop(pset_name, 
            " does not exist in the available PSets. Try one of the following:\n", 
            paste(availPSets$`PSet Name`, collapse = '\n'))
    }

    # Check if PSet exists in directories where PSets are stored. 
    # Read in if exists, download otherwise
    if (file.exists(file.path(psetDir, paste0(pset_name,".rds")))) {
        print("PSet exists in user-provided directory, reading .rds file")
        return(readRDS(file.path(psetDir, paste0(pset_name,".rds"))))
    } else{
        print("PSet does not exist in user-provided directory, downloading from database.")
        return(PharmacoGx::downloadPSet(pset_name, saveDir = psetDir, timeout = timeout))
    }

    # Update PSet to new structure if not updated already.
    PharmacoGx::updateObject(pset2)
}

#' Get dose and viability readouts and melt into large data table
#' @keywords internal
#' @importFrom data.table `:=` setDF
.extractDoseResponse <- function(PSet) {

        TRE <- PSet@treatmentResponse
        raw_tr <- TRE$raw

        # use output of get_env_identifiers() 
        env_ids <- gDRutils::get_env_identifiers()
    
        # Determine how many doses there are
        raw_tr_dims <- dim(raw_tr)
        if (raw_tr_dims[3] < 2) {
            stop("Error: PharmacoSet treatmentResponse raw data does not have either/both of viability and concentration data.")
        } else {
            viability <- data.table::as.data.table(raw_tr[, 1:raw_tr_dims[2], 2], TRUE)
            viability[[env_ids$untreated_tag[1]]] <- 100
            viability.m <- data.table::melt(viability, 
                        measure.vars = c(2:length(viability)),
                        variable.name = "Dose",
                        value.name = "ReadoutValue")

            doses <- data.table::as.data.table(raw_tr[, 1: raw_tr_dims[2],1], TRUE)
            doses[[env_ids$untreated_tag[1]]] <- 0
            doses.m <- data.table::melt(doses, 
                        measure.vars = c(2:length(doses)),
                        variable.name = "Dose",
                        value.name = env_ids$concentration)
        }
        
        # CHECK IF SAME SIZE
        # MERGE     
        if (length(doses.m) == length(viability.m)) {
            merged_dt <- merge(viability.m, doses.m)
        }else{
            print("doses and viability data tables are not the same size")
        }

        number_of_underscores <- sum(gregexpr("_", merged_dt$rn, fixed = TRUE)[[1]] > 0)
        merged_dt[, c( env_ids$drug_name , env_ids$cellline ) := data.table::tstrsplit(rn, "_", fixed = TRUE, keep = c(number_of_underscores,number_of_underscores+1))]

        merged_dt[Dose == env_ids$untreated_tag[1], env_ids$drug_name := env_ids$untreated_tag[1]]
        # implicit return
        merged_dt
}

#' Add in pseudo-data for duration and cell reference division time
#' @keywords internal
.createPseudoData <- function(dt) {
    if (!"Duration" %in% names(dt)) {
        dt$"Duration"<-72
    }
    if (!"ReferenceDivisionTime" %in% names(dt)) {
        dt$"ReferenceDivisionTime" <- NA
    }
    if (!"Barcode" %in% names(dt)) {
        colnames(dt)[1] <- "Barcode"
    }
    dt
}

#' Set parallel or serial
#' @keywords internal
.setParallel <- function(runParallel = TRUE, workers_ = 2L) {
    if (runParallel) {
        if (!is(BiocParallel::registered()[[1]],"MulticoreParam") | !(BiocParallel::registered()[[1]]$workers == workers_)) {
            print(paste("Setting environment as parallel with", workers_, "workers."))
            #run parallel
            BiocParallel::register( 
                BiocParallel::MulticoreParam(
                workers = as.numeric(workers_),
                tasks = 1L)
            )
        } else{
            print(paste("Environment set as parallel with", workers_, "workers."))
        }
        
    } else{
        print("Setting as serial.")
        # run serial
        BiocParallel::register(BiocParallel::SerialParam())
    }
}

#' Remove negative viabilities
#' @keywords internal
.removeNegatives <- function(dataset) {
    newdataset <- dataset[dataset$ReadoutValue>0]
}
