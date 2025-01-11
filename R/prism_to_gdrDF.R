#' Load, convert and process the level 5 PRISM data into a gDR input
#'
#' @param prism_data_path path to PRISM LEVEL5 csv file with data
#' @param readout_min minimum ReadoutValue
#'
#' @return \code{data.table} object with input data for gDR pipeline
#' @keywords prism_conversion
#' 
#' @examples
#'  prism_data <- system.file("testdata/prism_sa.csv", package = "gDRimport")
#'  convert_LEVEL5_prism_to_gDR_input(prism_data)
#'
#' @export
convert_LEVEL5_prism_to_gDR_input <- function(prism_data_path,
                                              readout_min = 1.03) {

  checkmate::check_file_exists(prism_data_path)
  
  data <- data.table::fread(prism_data_path)
  
  # Define the mapping for old column names
  column_mappings <- list(
    LFC_cb = c("LFC_cb", "LFC.cb", "LFC"),
    pert_iname = c("pert_iname", "pert_name")
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
  
  checkmate::assert_names(names(data), must.include = c("ccle_name",
                                                        "pert_iname",
                                                        "pert_dose",
                                                        "pert_time",
                                                        "LFC_cb"))
  gDRutils::reset_env_identifiers()
  idfs <- gDRutils::get_env_identifiers()
  
  # Check and split pert_iname and pert_dose by | or _
  if (any(grepl("\\|", data$pert_iname))) {
    separator <- "|"
  } else {
    separator <- "_"
  }
  data[, unlist(idfs[c("drug", "drug2")]) :=
         data.table::tstrsplit(data$pert_iname, separator, fixed = TRUE)]
  data[, unlist(idfs[c("concentration", "concentration2")]) :=
         data.table::tstrsplit(data$pert_dose, separator, fixed = TRUE, type.convert = TRUE)]

  raw_data <- data.table::data.table(clid = data$ccle_name,
                                     Duration = as.numeric(ifelse(grepl("d", data$pert_time),
                                                                  as.numeric(gsub("d", "", data$pert_time)) * 24,
                                                                  gsub("H", "", data$pert_time))),
                                     ReadoutValue = pmin(readout_min, 2 ^ data$LFC_cb),
                                     BackgroundValue = 0,
                                     Gnumber = data[[idfs$drug]],
                                     Gnumber_2 = data[[idfs$drug2]],
                                     Concentration = data[[idfs$concentration]],
                                     Concentration_2 = data[[idfs$concentration2]],
                                     masked = FALSE)

  data.table::setnames(raw_data, c("clid", "Duration", "Gnumber", "Gnumber_2",
                                   "Concentration", "Concentration_2", "masked"),
                       unlist(idfs[c("cellline", "duration", "drug", "drug2",
                                     "concentration", "concentration2", "masked_tag")]))

  if (all(raw_data[[idfs$drug]] == raw_data[[idfs$drug2]]) &&
      all(raw_data[[idfs$concentration]] == raw_data[[idfs$concentration2]])) {
    raw_data[,  unlist(idfs[c("drug2", "concentration2")]) := NULL]
  }

  dt_ctrl <- data.table::data.table(clid = unique(raw_data$clid),
                                    Gnumber = gDRutils::get_env_identifiers("untreated_tag")[1],
                                    Gnumber_2 = gDRutils::get_env_identifiers("untreated_tag")[1],
                                    Duration = unique(raw_data$Duration),
                                    Concentration = 0,
                                    Concentration_2 = 0,
                                    ReadoutValue = 1,
                                    BackgroundValue = 0,
                                    masked = FALSE)

  data.table::setnames(dt_ctrl, c("clid", "Duration", "Gnumber", "Gnumber_2",
                                  "Concentration", "Concentration_2", "masked"),
                       unlist(idfs[c("cellline", "duration", "drug", "drug2",
                                     "concentration", "concentration2", "masked_tag")]))

  if (!all(unlist(idfs[c("drug2", "concentration2")]) %in% names(raw_data))) {
    dt_ctrl[, unlist(idfs[c("drug2", "concentration2")]) := NULL]
  }
  rbind(raw_data, dt_ctrl)
}



#' Load, convert and process the level 6 PRISM data into a gDR input
#'
#' @param prism_data_path path to PRISM LEVEL6 csv file with collapsed
#' log fold change data
#' @param cell_line_data_path path to cell line info data
#' @param treatment_data_path path to collapsed treatment info data
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
#'  convert_LEVEL6_prism_to_gDR_input(prism_data_path, cell_line_data_path, treatment_data_path)
#'
#' @export
convert_LEVEL6_prism_to_gDR_input <- function(prism_data_path,
                                              cell_line_data_path,
                                              treatment_data_path,
                                              readout_min = 1.03) {

  checkmate::check_file_exists(prism_data_path)
  checkmate::check_file_exists(cell_line_data_path)
  checkmate::check_file_exists(treatment_data_path)

  cell_lines <- data.table::fread(cell_line_data_path)
  treatment <- data.table::fread(treatment_data_path)
  res <- data.table::fread(prism_data_path)
  
  data.table::setnames(cell_lines,
                       c("row_id", "depmap_id"),
                       c("row_name", "row_name"),
                       skip_absent = TRUE)
  data.table::setnames(treatment,
                       c("profile_id", "SampleID", "CompoundName", "GeneSymbolOfTargets"),
                       c("column_name", "column_name", "name", "moa"),
                       skip_absent = TRUE)
  
  # take into account different runs
  treatment[, new_name := if(.N > 1) paste0(name, "_run",
                                            data.table::frank(column_name, ties.method = "dense")) else as.character(name),
            by = name]
  treatment[, name := new_name]
  treatment[, new_name := NULL]

  checkmate::assert_names(names(cell_lines), must.include = c("row_name",
                                                              "ccle_name"))
  checkmate::assert_names(names(treatment), must.include = "column_name")
  checkmate::assert_names(names(res), must.include = "V1")


  res_transform <- data.table::melt(res, id.vars = "V1")
  data.table::setnames(res_transform,
                       c("V1", "variable"),
                       c("row_name", "column_name"))
  
  
  if (all(grepl("::", res_transform$column_name)) && !"dose" %in% names(treatment)) {
    res_transform[, c("column_name", "dose") := data.table::tstrsplit(column_name, "::", keep = c(1, 2))]
  }

  
  data.table::setnames(cell_lines,
                       "ccle_name",
                       "clid")

  full_data <- merge(res_transform,
                     unique(cell_lines[, c("row_name", "clid")]),
                     all.x = TRUE,
                     by = "row_name")

  full_data <- merge(full_data,
                     unique(treatment[, intersect(names(treatment),
                                           c("column_name", "name", "dose", "moa")),
                               with = FALSE]),
                     all.x = TRUE,
                     by = "column_name")

  full_data$value <- pmin(readout_min, 2 ^ full_data$value)
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
  # data for treatment
  ls_col <- intersect(c("clid", "name", "moa", "dose", "value"), colnames(full_data))
  dt_trt <- data.table::copy(full_data[, c(ls_col), with = FALSE])
  if (is.null(dt_trt$moa)) dt_trt$moa <- "unknown"
  data.table::setnames(dt_trt,
                       old = c("name", "moa", "dose", "value"),
                       new = c("Gnumber", "drug_moa", "Concentration", "ReadoutValue"))
  dt_trt$DrugName <- full_data$name
  dt_trt$Duration <- 120
  dt_trt$masked <- FALSE
  data.table::setcolorder(dt_trt, neworder = colnames(dt_ctrl))
  
  # final
  gDRutils::reset_env_identifiers()
  idfs <- gDRutils::get_env_identifiers()
  merged_data <- rbind(dt_trt, dt_ctrl)
  data.table::setnames(merged_data,
                       c("clid", "Gnumber", "DrugName", "drug_moa",
                         "Duration", "Concentration", "masked"),
                       unlist(idfs[c("cellline", "drug", "drug_name",
                                     "drug_moa", "duration", "concentration",
                                     "masked_tag")]))


  merged_data
}
