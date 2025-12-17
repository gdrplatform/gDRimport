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
  column_mappings <- list(
    LFC_cb = c("LFC_cb", "LFC.cb", "LFC", "l2fc"),
    pert_iname = c("pert_iname", "pert_name"),
    pert_time = c("pert_time", "day"),
    pert2_iname = c("pert2_name", "pert2_iname", "drug2", "compound2"),
    pert2_dose = c("pert2_dose", "concentration2", "dose2")
  )
  
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
  
  data <- meta[, .SD, .SDcols =  c("ModelID",
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
    raw_data[,  unlist(idfs[c("drug2", "concentration2")]) := NULL]
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
  rbind(raw_data, dt_ctrl)
}



#' Load, convert and process the level 6 PRISM data into a gDR input
#'
#' @param prism_data_path path to PRISM LEVEL6 csv file with collapsed
#' log fold change data
#' @param cell_line_data_path path to cell line info data
#' @param treatment_data_path path to collapsed treatment info data
#' @param meta_data_path path to metadata file describing all cancer models/cell lines
#' which are referenced by a dataset contained within the DepMap portal
#' @param readout_min minimum ReadoutValue
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
                                              cell_line_data_path,
                                              treatment_data_path,
                                              meta_data_path,
                                              readout_min = 1.03) {
  
  checkmate::assert_file_exists(prism_data_path)
  checkmate::assert_file_exists(cell_line_data_path)
  
  checkmate::assert_file_exists(treatment_data_path)
  checkmate::assert_file_exists(meta_data_path)
  
  gDRutils::reset_env_identifiers()
  idfs <- gDRutils::get_env_identifiers()
  
  cell_lines <- data.table::fread(cell_line_data_path)
  treatment <- data.table::fread(treatment_data_path)
  res <- data.table::fread(prism_data_path)
  meta <- data.table::fread(meta_data_path)
  
  checkmate::assert_names(names(meta),
                          must.include = c("ModelID",
                                           "CCLEName",
                                           "OncotreeLineage"))
  # rename cell_lines and treatment
  if ("depmap_id" %in% names(cell_lines)) {
    data.table::setnames(cell_lines, "depmap_id", "row_name", skip_absent = TRUE)
  } else {
    data.table::setnames(cell_lines, "row_id", "row_name", skip_absent = TRUE)
  }
  data.table::setnames(treatment,
                       c("profile_id", "SampleID", "CompoundName", "GeneSymbolOfTargets"),
                       c("column_name", "column_name", "name", "moa"),
                       skip_absent = TRUE)
  
  checkmate::assert_names(names(cell_lines), must.include = "row_name")
  checkmate::assert_names(names(treatment), must.include = "column_name")
  
  if (!"LFC" %in% names(res)) {
    checkmate::assert_names(names(res), must.include = "V1")
    res <- data.table::melt(res, id.vars = "V1")
    data.table::setnames(res,
                         c("V1", "variable"),
                         c("row_name", "column_name"))
    
    if (all(grepl("::", res$column_name)) && !"dose" %in% names(treatment)) {
      res[, c("column_name", "dose") := data.table::tstrsplit(column_name, "::", keep = c(1, 2))]
    }
  } else {
    data.table::setnames(res, c("row_id", "profile_id"), c("row_name", "column_name"))
  }
  
  # add meta data to cell_line
  cell_lines <- meta[, .SD, .SDcols = c("ModelID",
                                        "CCLEName",
                                        "OncotreeLineage")][cell_lines, on = .(ModelID = row_name)]
  cell_lines[, unlist(idfs[c("cellline_parental_identifier", "cellline_subtype", "cellline_ref_div_time")]) :=
               list("unknown", "unknown", as.numeric(NA))]
  cell_lines[, (idfs[["cellline_name"]]) := CCLEName]
  cell_lines[, OncotreeLineage := data.table::fcoalesce(OncotreeLineage, "unknown")]
  # rename cell_lines 
  data.table::setnames(cell_lines,
                       c("CCLEName", "OncotreeLineage"),
                       unlist(idfs[c("cellline", "cellline_tissue")]))
  
  # merge results with cell_line data
  res$row_name <- gsub("::.*", "", res$row_name)
  
  full_data <- merge(res,
                     unique(cell_lines[, c("ModelID",
                                           unlist(idfs[c("cellline",
                                                         "cellline_name",
                                                         "cellline_tissue",
                                                         "cellline_parental_identifier",
                                                         "cellline_subtype",
                                                         "cellline_ref_div_time")])), with = FALSE]),
                     all.x = TRUE,
                     by.x = "row_name",
                     by.y = "ModelID")
  
  
  # merge results with treatment data
  full_data <- merge(full_data,
                     unique(treatment[, intersect(names(treatment),
                                                  c("column_name", "name", "dose", "broad_id", "moa")),
                                      with = FALSE]),
                     all.x = TRUE,
                     by = "column_name")
  value_col <- if ("LFC" %in% names(full_data)) {
    "LFC"
  } else {
    "value"
  }
  full_data$value <- pmin(readout_min, 2 ^ full_data[[value_col]])
  full_data <- full_data[!(is.na(name) | is.na(value))]
  
  # data for conc = 0
  untrt_tag <- gDRutils::get_env_identifiers("untreated_tag")[1]
  dt_ctrl <- data.table::data.table(clid = unique(full_data$clid),
                                    Gnumber = untrt_tag,
                                    DrugName = untrt_tag,
                                    drug_moa = untrt_tag,
                                    Duration = 120,
                                    Concentration = 0,
                                    ReadoutValue = 1,
                                    masked = FALSE)
  # control data
  dt_ctrl <- merge(dt_ctrl,
                   unique(full_data[, unlist(idfs[c("cellline", "cellline_name", "cellline_tissue",
                                                    "cellline_parental_identifier", "cellline_subtype",
                                                    "cellline_ref_div_time")]), with = FALSE]), all.x = TRUE)
  
  # data for treatment
  ls_col <- intersect(c("clid", "name", "broad_id", "moa", "dose", "value",
                        unlist(idfs[c("cellline", "cellline_name", "cellline_tissue",
                                      "cellline_parental_identifier", "cellline_subtype",
                                      "cellline_ref_div_time")])),
                      colnames(full_data))
  dt_trt <- data.table::copy(full_data[, c(ls_col), with = FALSE])
  if (is.null(dt_trt$moa)) dt_trt$moa <- "unknown"
  data.table::setnames(dt_trt,
                       old = c("broad_id", "name", "moa", "dose", "value"),
                       new = c("Gnumber", "DrugName", "drug_moa", "Concentration", "ReadoutValue"))
  dt_trt$Duration <- 120
  dt_trt$masked <- FALSE
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
  merged_data
}
