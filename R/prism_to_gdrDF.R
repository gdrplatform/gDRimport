# Column names the LEVEL5 converter accepts for each column it needs; the first
# synonym present in the data is renamed when the column itself is missing.
# validate_gdr_input() checks PRISM files against the same list.
#' @keywords internal
.prism_level5_column_synonyms <- function() {
  list(
    LFC_cb = c("LFC_cb", "LFC.cb", "LFC", "l2fc"),
    pert_iname = c("pert_iname", "pert_name"),
    pert_time = c("pert_time", "day"),
    pert2_iname = c("pert2_name", "pert2_iname", "drug2", "compound2"),
    pert2_dose = c("pert2_dose", "concentration2", "dose2")
  )
}


#' Load, convert and process the level 5 PRISM data into a gDR input
#'
#' @param prism_data_path path to PRISM LEVEL5 csv file with data
#' @param meta_data_path path to metadata file describing all cancer models/cell lines
#' which are referenced by a dataset contained within the DepMap portal
#' @param readout_min minimum ReadoutValue
#'
#' @return \code{data.table} object with input data for gDR pipeline
#' @keywords prism_conversion
#'
#' @examples
#'  prism_data <- system.file("testdata/prism_sa.csv", package = "gDRimport")
#'  prism_meta <- system.file("testdata/prism_model.csv", package = "gDRimport")
#'  convert_LEVEL5_prism_to_gDR_input(prism_data, prism_meta)
#'
#' @export
convert_LEVEL5_prism_to_gDR_input <- function(prism_data_path,
                                              meta_data_path,
                                              readout_min = 1.03) {

  checkmate::check_file_exists(prism_data_path)
  gDRutils::reset_env_identifiers()
  idfs <- gDRutils::get_env_identifiers()

  data <- data.table::fread(prism_data_path)
  meta <- data.table::fread(meta_data_path)

  checkmate::assert_names(names(meta),
                          must.include = c("ModelID",
                                           "CCLEName",
                                           "OncotreeLineage"))

  # Define the mapping for old column names
  column_mappings <- .prism_level5_column_synonyms()

  # Rename columns based on mapping if default column is missing
  for (col in names(column_mappings)) {
    if (!col %in% names(data)) {
      for (old_col in column_mappings[[col]]) {
        if (old_col %in% names(data)) {
          data.table::setnames(data, old_col, col)
          break
        }
      }
    }
  }

  if (!"ccle_name" %in% names(data)) {
    data$ccle_name <- meta$CCLEName[match(data$depmap_id, meta$ModelID)]
  }

  checkmate::assert_names(names(data), must.include = c("ccle_name",
                                                        "pert_iname",
                                                        "pert_dose",
                                                        "pert_time",
                                                        "LFC_cb"))

  data <- data[data$ccle_name != "", ]

  if ("pert2_iname" %in% names(data) && "pert2_dose" %in% names(data)) {
    data[, (idfs$drug) := pert_iname]
    data[, (idfs$concentration) := as.numeric(pert_dose)]

    data[, (idfs$drug2) := pert2_iname]
    data[, (idfs$concentration2) := as.numeric(pert2_dose)]

  } else {
    if (any(grepl("\\|", data$pert_iname))) {
      separator <- "|"
    } else {
      separator <- "_"
    }

    data[, unlist(idfs[c("drug", "drug2")]) :=
           data.table::tstrsplit(data$pert_iname, separator, fixed = TRUE)]

    data[, unlist(idfs[c("concentration", "concentration2")]) :=
           data.table::tstrsplit(data$pert_dose, separator, fixed = TRUE, type.convert = TRUE)]
  }

  data <- meta[, .SD, .SDcols = c("ModelID",
                                   "CCLEName",
                                   "OncotreeLineage")][data, on = .(CCLEName = ccle_name)]

  data[, unlist(idfs[c("cellline_parental_identifier", "cellline_subtype", "cellline_ref_div_time")]) :=
         list("unknown", "unknown", as.numeric(NA))]

  raw_data <- data.table::data.table(clid = data$CCLEName,
                                     CellLineName = data$CCLEName,
                                     Tissue = ifelse(is.na(data$OncotreeLineage),
                                                     "unknown",
                                                     data$OncotreeLineage),
                                     parental_identifier = data$parental_identifier,
                                     subtype = data$subtype,
                                     ReferenceDivisionTime = data$ReferenceDivisionTime,
                                     Duration = as.numeric(ifelse(grepl("d|^\\d+$", data$pert_time),
                                                                  as.numeric(gsub("d", "", data$pert_time)) * 24,
                                                                  gsub("H", "", data$pert_time))),
                                     ReadoutValue = pmin(readout_min, 2 ^ data$LFC_cb),
                                     BackgroundValue = 0,
                                     Gnumber = data[[idfs$drug]],
                                     Gnumber_2 = data[[idfs$drug2]],
                                     Concentration = data[[idfs$concentration]],
                                     Concentration_2 = data[[idfs$concentration2]],
                                     masked = FALSE)
  # rename columns of raw data
  data.table::setnames(raw_data, c("clid", "CellLineName", "Tissue", "parental_identifier",
                                   "subtype", "ReferenceDivisionTime", "Duration", "Gnumber",
                                   "Gnumber_2", "Concentration", "Concentration_2", "masked"),
                       unlist(idfs[c("cellline", "cellline_name", "cellline_tissue",
                                     "cellline_parental_identifier", "cellline_subtype",
                                     "cellline_ref_div_time", "duration", "drug", "drug2",
                                     "concentration", "concentration2", "masked_tag")]))
  # combo columns
  if (all(raw_data[[idfs$drug]] == raw_data[[idfs$drug2]]) &&
      all(raw_data[[idfs$concentration]] == raw_data[[idfs$concentration2]])) {
    raw_data[, unlist(idfs[c("drug2", "concentration2")]) := NULL]
  }

  # control data
  dt_ctrl <- data.table::data.table(clid = unique(raw_data$clid),
                                    Gnumber = gDRutils::get_env_identifiers("untreated_tag")[1],
                                    Gnumber_2 = gDRutils::get_env_identifiers("untreated_tag")[1],
                                    Duration = unique(raw_data$Duration),
                                    Concentration = 0,
                                    Concentration_2 = 0,
                                    ReadoutValue = 1,
                                    BackgroundValue = 0,
                                    masked = FALSE)
  # merge raw and control
  dt_ctrl <- merge(dt_ctrl,
                   unique(raw_data[, unlist(idfs[c("cellline", "cellline_name", "cellline_tissue",
                                                   "cellline_parental_identifier", "cellline_subtype",
                                                   "cellline_ref_div_time")]), with = FALSE]), all.x = TRUE)

  # rename columns of control data
  data.table::setnames(dt_ctrl, c("clid", "Duration", "Gnumber", "Gnumber_2",
                                  "Concentration", "Concentration_2", "masked"),
                       unlist(idfs[c("cellline", "duration", "drug", "drug2",
                                     "concentration", "concentration2", "masked_tag")]))
  # combo columns
  if (!all(unlist(idfs[c("drug2", "concentration2")]) %in% names(raw_data))) {
    dt_ctrl[, unlist(idfs[c("drug2", "concentration2")]) := NULL]
  }
  # final
  out <- rbind(raw_data, dt_ctrl)
  data.table::setattr(out, "gDR_import", .import_record(
    files = list(
      .import_files("prism_data", prism_data_path),
      .import_files("meta_data", meta_data_path)
    ),
    instrument = NULL,
    events = list(),
    source = list(format = "PRISM LEVEL5", readout_min = readout_min)
  ))
  out
}



# Column names the LEVEL6 converter accepts in the LFC data and in the treatment
# file; the first synonym present is renamed when the column itself is missing.
# Long LFC tables come as row_id/profile_id (Repurposing) or depmap_id/SampleID/pert_dose
# (DepMap OncRef from 25Q2, Luminex and sequencing readouts).
#' @keywords internal
.prism_level6_column_synonyms <- function() {
  list(
    data = list(
      row_name = c("row_id", "depmap_id"),
      column_name = c("profile_id", "SampleID"),
      dose = "pert_dose",
      LFC = c("LFC", "median_l2fc")
    ),
    treatment = list(
      column_name = c("profile_id", "SampleID", "pert_mfc_id"),
      name = c("CompoundName", "Compound name"),
      moa = c("TargetOrMechanism", "Target/ mechanism")
    )
  )
}


# Rename the first synonym present to its target, unless the target is already there.
#' @keywords internal
.rename_synonyms <- function(dt, synonyms) {
  for (target in names(synonyms)) {
    present <- intersect(synonyms[[target]], names(dt))
    if (!target %in% names(dt) && length(present) > 0L) {
      data.table::setnames(dt, present[1], target)
    }
  }
  dt
}


# DepMap model ID (ACH-...) from a PRISM cell line ID such as "ACH-000007",
# "PR500_ACH-000007" or "ACH-000007::P103::PR500A::REP1M"; IDs without an ACH
# number are returned without the "::" suffix.
#' @keywords internal
.prism_model_id <- function(x) {
  x <- sub("::.*", "", x)
  has_ach <- grepl("ACH-[0-9]+", x)
  x[has_ach] <- sub(".*(ACH-[0-9]+).*", "\\1", x[has_ach])
  x
}


#' Load, convert and process the level 6 PRISM data into a gDR input
#'
#' Reads collapsed log fold changes from the PRISM Repurposing screens (LFC matrix
#' or long \code{row_id}/\code{profile_id} table) and from the DepMap PRISM
#' Oncology Reference (LFC matrix up to 24Q4, long \code{LFCCollapsed} tables of the
#' Luminex and sequencing readouts from 25Q2).
#'
#' Cell lines are matched to \code{ModelID} in the model metadata by their DepMap
#' ID. Cell lines that failed STR profiling (\code{_FAILED_STR} in the ID) and cell
#' lines missing from the model metadata are dropped. The cell line ID is the
#' \code{CCLEName}, or the \code{ModelID} for models without one. Measurements that
#' DepMap flags as outliers are kept with \code{masked = TRUE}. Everything dropped or
#' masked is listed in \code{get_import_provenance(x)$corrections}.
#'
#' @param prism_data_path path to PRISM LEVEL6 csv file with collapsed
#' log fold change data
#' @param cell_line_data_path path to cell line info data, or \code{NULL}. Cell lines
#' are taken from the LFC data, so the file is only recorded in the import provenance;
#' the sequencing readout of DepMap OncRef comes without one.
#' @param treatment_data_path path to collapsed treatment info data
#' @param meta_data_path path to metadata file describing all cancer models/cell lines
#' which are referenced by a dataset contained within the DepMap portal
#' @param readout_min minimum ReadoutValue
#' @param dose_units dose units to keep when the data has a \code{pert_dose_unit}
#' column (DepMap OncRef from 25Q2 has antibodies and ADCs in ug/mL), or \code{NULL}
#' to keep all. gDR expects concentrations in uM.
#'
#' @return \code{data.table} object with input data for gDR pipeline
#'
#' @keywords prism_conversion
#'
#' @examples
#'  prism_data_path <- system.file("testdata/prism_collapsed_LOGFC.csv", package = "gDRimport")
#'  cell_line_data_path <- system.file("testdata/prism_cell_lines.csv", package = "gDRimport")
#'  treatment_data_path <- system.file("testdata/prism_treatment.csv", package = "gDRimport")
#'  prism_meta <- system.file("testdata/prism_model.csv", package = "gDRimport")
#'  convert_LEVEL6_prism_to_gDR_input(prism_data_path, cell_line_data_path, treatment_data_path, prism_meta)
#'
#' @export
convert_LEVEL6_prism_to_gDR_input <- function(prism_data_path,
                                              cell_line_data_path = NULL,
                                              treatment_data_path,
                                              meta_data_path,
                                              readout_min = 1.03,
                                              dose_units = "uM") {

  checkmate::assert_file_exists(prism_data_path)
  if (!is.null(cell_line_data_path)) checkmate::assert_file_exists(cell_line_data_path)
  checkmate::assert_file_exists(treatment_data_path)
  checkmate::assert_file_exists(meta_data_path)
  checkmate::assert_character(dose_units, null.ok = TRUE)

  gDRutils::reset_env_identifiers()
  idfs <- gDRutils::get_env_identifiers()
  synonyms <- .prism_level6_column_synonyms()
  events <- list()

  treatment <- data.table::fread(treatment_data_path)
  res <- data.table::fread(prism_data_path)
  meta <- data.table::fread(meta_data_path)

  checkmate::assert_names(names(meta),
                          must.include = c("ModelID",
                                           "CCLEName",
                                           "OncotreeLineage"))

  treatment <- .rename_synonyms(treatment, synonyms$treatment)
  checkmate::assert_names(names(treatment), must.include = "column_name")
  # OncRef compound lists have no broad_id; their sample ID identifies the compound
  if (!"broad_id" %in% names(treatment)) treatment[, broad_id := column_name]

  if (names(res)[1] %in% c("", "V1") && !any(synonyms$data$LFC %in% names(res))) {
    data.table::setnames(res, 1, "V1")
    res <- data.table::melt(res, id.vars = "V1")
    data.table::setnames(res,
                         c("V1", "variable"),
                         c("row_name", "column_name"))

    if (all(grepl("::", res$column_name)) && !"dose" %in% names(treatment)) {
      res[, c("column_name", "dose") := data.table::tstrsplit(column_name, "::", keep = c(1, 2))]
      res[, dose := as.numeric(dose)]
    }
  } else {
    res <- .rename_synonyms(res, synonyms$data)
    checkmate::assert_names(names(res), must.include = c("row_name", "column_name", "LFC"))
    if ("dose" %in% names(res) && "dose" %in% names(treatment)) treatment[, dose := NULL]
  }

  if (!is.null(dose_units) && "pert_dose_unit" %in% names(res)) {
    other_units <- !res$pert_dose_unit %in% dose_units
    if (any(other_units)) {
      events <- c(events, list(list(
        type = "prism_dose_units_dropped",
        units = sort(unique(res$pert_dose_unit[other_units])),
        compounds = sort(unique(as.character(res$column_name[other_units])))
      )))
      res <- res[!other_units]
    }
  }

  # cell lines: DepMap model ID from the data, names and lineage from the model metadata
  failed_str <- grepl("FAILED_STR", res$row_name)
  if (any(failed_str)) {
    events <- c(events, list(list(type = "prism_failed_str_dropped",
                                  ids = sort(unique(res$row_name[failed_str])))))
    res <- res[!failed_str]
  }
  res[, row_name := .prism_model_id(row_name)]
  unmapped <- setdiff(unique(res$row_name), meta$ModelID)
  if (length(unmapped) > 0L) {
    events <- c(events, list(list(type = "prism_cell_lines_unmapped", ids = sort(unmapped))))
    res <- res[!row_name %in% unmapped]
  }

  cell_lines <- unique(meta[ModelID %in% res$row_name, c("ModelID", "CCLEName", "OncotreeLineage")],
                       by = "ModelID")
  cell_lines[, CCLEName := data.table::fifelse(is.na(CCLEName) | CCLEName == "", ModelID, CCLEName)]
  cell_lines[, unlist(idfs[c("cellline_parental_identifier", "cellline_subtype", "cellline_ref_div_time")]) :=
               list("unknown", "unknown", as.numeric(NA))]
  cell_lines[, (idfs[["cellline_name"]]) := CCLEName]
  cell_lines[, OncotreeLineage := data.table::fifelse(is.na(OncotreeLineage) | OncotreeLineage == "",
                                                      "unknown", OncotreeLineage)]
  data.table::setnames(cell_lines,
                       c("CCLEName", "OncotreeLineage"),
                       unlist(idfs[c("cellline", "cellline_tissue")]))

  full_data <- merge(res, cell_lines, by.x = "row_name", by.y = "ModelID")

  # merge results with treatment data; one row per compound so the merge adds no rows
  full_data <- merge(full_data,
                     unique(treatment[, intersect(names(treatment),
                                                  c("column_name", "name", "dose", "broad_id", "moa")),
                                      with = FALSE], by = "column_name"),
                     all.x = TRUE,
                     by = "column_name")
  value_col <- if ("LFC" %in% names(full_data)) {
    "LFC"
  } else {
    "value"
  }
  full_data$value <- pmin(readout_min, 2 ^ full_data[[value_col]])
  full_data <- full_data[!(is.na(name) | is.na(value))]

  full_data$masked <- if ("outlier" %in% names(full_data)) {
    data.table::fcoalesce(as.logical(full_data$outlier), FALSE)
  } else {
    FALSE
  }
  if (any(full_data$masked)) {
    events <- c(events, list(list(type = "prism_outliers_masked", n = sum(full_data$masked))))
  }

  # data for conc = 0
  untrt_tag <- gDRutils::get_env_identifiers("untreated_tag")[1]
  dt_ctrl <- data.table::data.table(clid = unique(full_data$clid),
                                    Gnumber = untrt_tag,
                                    DrugName = untrt_tag,
                                    drug_moa = untrt_tag,
                                    Duration = NA_real_,
                                    Concentration = 0,
                                    ReadoutValue = 1,
                                    masked = FALSE)
  # control data
  dt_ctrl <- merge(dt_ctrl,
                   unique(full_data[, unlist(idfs[c("cellline", "cellline_name", "cellline_tissue",
                                                    "cellline_parental_identifier", "cellline_subtype",
                                                    "cellline_ref_div_time")]), with = FALSE]), all.x = TRUE)

  # data for treatment
  ls_col <- intersect(c("clid", "name", "broad_id", "moa", "dose", "value", "masked",
                        unlist(idfs[c("cellline", "cellline_name", "cellline_tissue",
                                      "cellline_parental_identifier", "cellline_subtype",
                                      "cellline_ref_div_time")])),
                      colnames(full_data))
  dt_trt <- data.table::copy(full_data[, c(ls_col), with = FALSE])
  if (is.null(dt_trt$moa)) dt_trt$moa <- "unknown"
  data.table::setnames(dt_trt,
                       old = c("broad_id", "name", "moa", "dose", "value"),
                       new = c("Gnumber", "DrugName", "drug_moa", "Concentration", "ReadoutValue"))
  dt_trt$Duration <- NA_real_
  data.table::setcolorder(dt_trt, neworder = colnames(dt_ctrl))

  # merge treatment and control
  merged_data <- rbind(dt_trt, dt_ctrl)
  data.table::setnames(merged_data,
                       c("clid", "Gnumber", "DrugName", "drug_moa",
                         "Duration", "Concentration", "masked"),
                       unlist(idfs[c("cellline", "drug", "drug_name",
                                     "drug_moa", "duration", "concentration",
                                     "masked_tag")]))

  # final
  data.table::setattr(merged_data, "gDR_import", .import_record(
    files = list(
      .import_files("prism_data", prism_data_path),
      .import_files("cell_line_data", cell_line_data_path),
      .import_files("treatment_data", treatment_data_path),
      .import_files("meta_data", meta_data_path)
    ),
    instrument = NULL,
    events = events,
    source = list(format = "PRISM LEVEL6", readout_min = readout_min, dose_units = dose_units)
  ))
  merged_data
}
