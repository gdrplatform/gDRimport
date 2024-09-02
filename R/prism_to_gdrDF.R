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
  checkmate::assert_names(names(data), must.include = c("rid", "ccle_name", "culture", "pool_id",
                                                     "pert_iname", "pert_id", "pert_dose",
                                                     "pert_idose", "pert_plate", "pert_vehicle",
                                                     "pert_time", "pert_type", "sig_id",
                                                     "x_project_id", "LFC", "LFC_cb"))
  gDRutils::reset_env_identifiers()
  idfs <- gDRutils::get_env_identifiers()

  data[, unlist(idfs[c("drug", "drug2")]) := data.table::tstrsplit(data$pert_iname, "|", fixed = TRUE)]
  data[, unlist(idfs[c("concentration", "concentration2")]) :=
         data.table::tstrsplit(data$pert_dose, "|", fixed = TRUE, type.convert = TRUE)]

  raw_data <- data.table::data.table(clid = data$ccle_name,
                                     Duration = as.numeric(gsub("H", "", data$pert_time)),
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

  df_ctrl <- data.table::data.table(clid = unique(raw_data$clid),
                                    Gnumber = gDRutils::get_env_identifiers("untreated_tag")[1],
                                    Gnumber_2 = gDRutils::get_env_identifiers("untreated_tag")[1],
                                    Duration = unique(raw_data$Duration),
                                    Concentration = 0,
                                    Concentration_2 = 0,
                                    ReadoutValue = 1,
                                    BackgroundValue = 0,
                                    masked = FALSE)

  data.table::setnames(df_ctrl, c("clid", "Duration", "Gnumber", "Gnumber_2",
                                  "Concentration", "Concentration_2", "masked"),
                       unlist(idfs[c("cellline", "duration", "drug", "drug2",
                                     "concentration", "concentration2", "masked_tag")]))

  if (!all(unlist(idfs[c("drug2", "concentration2")]) %in% names(raw_data))) {
    df_ctrl[, unlist(idfs[c("drug2", "concentration2")]) := NULL]
  }
  rbind(raw_data, df_ctrl)
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
  
  data.table::setnames(cell_lines, "row_id", "row_name", skip_absent = TRUE)
  data.table::setnames(treatment, "profile_id", "column_name", skip_absent = TRUE)

  checkmate::assert_names(names(cell_lines), must.include = c("row_name",
                                                              "ccle_name"))
  checkmate::assert_names(names(treatment), must.include = c("column_name",
                                                              "broad_id"))
  checkmate::assert_names(names(res), must.include = "V1")


  res_transform <- data.table::melt(res, id.vars = "V1")
  data.table::setnames(res_transform,
                       c("V1", "variable"),
                       c("row_name", "column_name"))
  
  data.table::setnames(cell_lines,
                       "ccle_name",
                       "clid")

  full_data <- merge(res_transform, cell_lines[, c("row_name", "clid")],
                     all.x = TRUE,
                     by = "row_name")

  full_data <- merge(full_data,
                     treatment[, intersect(names(treatment),
                                           c("column_name", "broad_id", "name", "dose", "moa")),
                               with = FALSE],
                     all.x = TRUE,
                     by = "column_name")

  full_data$value <- pmin(readout_min, 2 ^ full_data$value)
  full_data <- full_data[!(is.na(name) | is.na(value) | name == "DMSO")]
  
  untrt_tag <- gDRutils::get_env_identifiers("untreated_tag")[1]

  df_ctrl <- data.table::data.table(clid = unique(full_data$clid),
                                    Gnumber = untrt_tag,
                                    DrugName = untrt_tag,
                                    drug_moa = untrt_tag,
                                    Duration = 120,
                                    Concentration = 0,
                                    ReadoutValue = 1,
                                    masked = FALSE)
  df_trt <- data.table::data.table(clid = full_data$clid,
                                   Gnumber = full_data$broad_id,
                                   DrugName = full_data$name,
                                   drug_moa = ifelse(is.null(full_data$moa),
                                                     "unknown",
                                                     full_data$moa),
                                   Duration = 120,
                                   Concentration = full_data$dose,
                                   ReadoutValue = full_data$value,
                                   masked = FALSE)
  gDRutils::reset_env_identifiers()
  idfs <- gDRutils::get_env_identifiers()
  merged_data <- rbind(df_trt, df_ctrl)
  data.table::setnames(merged_data,
                       c("clid", "Gnumber", "DrugName", "drug_moa",
                         "Duration", "Concentration", "masked"),
                       unlist(idfs[c("cellline", "drug", "drug_name",
                                     "drug_moa", "duration", "concentration",
                                     "masked_tag")]))


  merged_data
}
