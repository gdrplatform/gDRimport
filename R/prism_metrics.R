#' Add the curve fits delivered by PRISM to the Metrics assay
#'
#' Reads a PRISM dose-response table (the \code{DRC_TABLE} of an MTS or EPS delivery)
#' and adds every curve in it to the \code{Metrics} assay of a processed PRISM
#' \code{SummarizedExperiment}, with \code{fit_source = "PRISM"}, next to the gDR fits.
#' The curves are imported as PRISM fitted them, without refitting, so the values match
#' the delivery exactly.
#'
#' PRISM fits \code{upper_limit + (lower_limit - upper_limit) / (1 + (dose / inflection)^slope)},
#' which is the gDR curve with \code{x_0 = upper_limit}, \code{x_inf = lower_limit},
#' \code{ec50 = inflection} and \code{h = -slope}. \code{r2} is \code{frac_var_explained} and
#' \code{fit_type} is PRISM's \code{fit_name}. PRISM defines AUC and IC50 differently from gDR
#' (the curve is clamped between 0 and 1, an IC50 outside the tested range is \code{NA}), so those
#' values are kept under their own names (\code{prism_auc}, \code{prism_ic50} and so on) and the
#' gDR columns \code{x_AOC}, \code{xc50}, \code{x_mean} and \code{x_max} stay \code{NA}.
#'
#' A PRISM screen often has several curves for one cell line and compound (different pools or
#' compound plates), which gDR averages into one cell. Each of them becomes its own row, with
#' \code{depmap_id}, \code{pert_id}, \code{pool_id}, \code{lua}, \code{cell_set} and
#' \code{pert_plate} to tell them apart. Curves
#' with no matching cell in \code{se} are left out with a warning. Running the import again
#' replaces the PRISM rows of the earlier run.
#'
#' Run it after the processing: \code{fit_SE()} replaces the whole \code{Metrics} assay.
#'
#' @param se \code{SummarizedExperiment} of single-agent PRISM data, processed by gDR.
#' @param drc_path string, path to the PRISM DRC table (CSV).
#' @param meta_data_path string, path to the DepMap \code{Model.csv} that maps \code{depmap_id}
#'   to the CCLE name used as cell line identifier (the file given to
#'   \code{\link{convert_LEVEL5_prism_to_gDR_input}}). Not needed when the table has a
#'   \code{ccle_name} column.
#' @param drug_map named character vector mapping PRISM's \code{pert_name} to the drug
#'   identifier in \code{se}, for data imported with other drug identifiers. By default the
#'   \code{pert_name} is the drug identifier, as in \code{convert_LEVEL5_prism_to_gDR_input()}.
#' @param normalization_type string, the normalization type of the PRISM curves.
#' @param metrics_assay string, name of the assay with the fits.
#' @param averaged_assay string, name of the assay used to count the concentrations
#'   (\code{N_conc}).
#'
#' @return \code{se} with the PRISM curves in \code{metrics_assay} and the import recorded in its
#'   provenance (fit source \code{"PRISM"} and step \code{"prism_drc_import"}).
#'
#' @examples
#' mae <- gDRutils::get_synthetic_data("finalMAE_prism")
#' se <- MultiAssayExperiment::experiments(mae)[["single-agent"]]
#' se <- add_PRISM_metrics(
#'   se,
#'   drc_path = system.file("testdata/prism_drc.csv", package = "gDRimport"),
#'   meta_data_path = system.file("testdata/prism_drc_model.csv", package = "gDRimport")
#' )
#' metrics <- gDRutils::convert_se_assay_to_dt(se, "Metrics")
#' metrics[metrics$fit_source == "PRISM", ]
#'
#' @keywords prism_conversion
#' @export
add_PRISM_metrics <- function(se,
                              drc_path,
                              meta_data_path = NULL,
                              drug_map = NULL,
                              normalization_type = "RV",
                              metrics_assay = "Metrics",
                              averaged_assay = "Averaged") {
  checkmate::assert_class(se, "SummarizedExperiment")
  checkmate::assert_file_exists(drc_path)
  if (!is.null(meta_data_path)) checkmate::assert_file_exists(meta_data_path)
  checkmate::assert_character(drug_map, names = "unique", any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_string(normalization_type)
  checkmate::assert_string(metrics_assay)
  checkmate::assert_string(averaged_assay)

  drc <- data.table::fread(drc_path, colClasses = list(character = c("pert_name", "depmap_id")))
  checkmate::assert_names(names(drc), must.include = c(
    .prism_drc_fit_columns(), "pert_name", "day", "fit_name", "frac_var_explained", "maximum_dose"
  ))
  n_curves <- NROW(drc)

  # cell line: the CCLE name, as convert_LEVEL5_prism_to_gDR_input() uses it
  if (!"ccle_name" %in% names(drc)) {
    checkmate::assert_names(names(drc), must.include = "depmap_id")
    if (is.null(meta_data_path)) {
      stop("'meta_data_path' is needed to map depmap_id to the cell line identifier")
    }
    meta <- data.table::fread(meta_data_path)
    checkmate::assert_names(names(meta), must.include = c("ModelID", "CCLEName"))
    drc$ccle_name <- meta$CCLEName[match(drc$depmap_id, meta$ModelID)]
  }
  drug <- if (is.null(drug_map)) drc$pert_name else unname(drug_map[drc$pert_name])

  idfs <- gDRutils::get_SE_identifiers(se, simplify = TRUE)
  rdata <- data.table::as.data.table(SummarizedExperiment::rowData(se))
  cdata <- data.table::as.data.table(SummarizedExperiment::colData(se))
  rdata$row <- rownames(se)
  cdata$column <- colnames(se)
  # PRISM curves are single-agent: leave out rows with a second drug
  drug2 <- idfs[["drug2"]]
  if (drug2 %in% names(rdata)) {
    untreated <- gDRutils::get_env_identifiers("untreated_tag")
    rdata <- rdata[is.na(rdata[[drug2]]) | rdata[[drug2]] %in% untreated]
  }
  row_key <- paste(rdata[[idfs[["drug"]]]], as.numeric(rdata[[idfs[["duration"]]]]))
  drc_row_key <- paste(drug, as.numeric(drc$day) * 24)
  drc$row <- rdata$row[match(drc_row_key, row_key)]
  drc$column <- cdata$column[match(drc$ccle_name, cdata[[idfs[["cellline"]]]])]

  matched <- !is.na(drc$row) & !is.na(drc$column)
  if (!all(matched)) {
    warning(sprintf(
      paste("%d of %d PRISM curves have no matching cell in the SE and are left out",
            "(%d without a cell line match, %d without a drug or Duration match)"),
      sum(!matched), n_curves, sum(is.na(drc$column)), sum(is.na(drc$row))
    ))
  }
  if (!any(matched)) {
    stop("no PRISM curve matches a cell in the SE: check the drug identifiers ('drug_map') ",
         "and the cell line mapping")
  }
  drc <- drc[matched]

  new_dt <- .prism_curves_to_metrics(drc, normalization_type)
  new_dt$N_conc <- .count_concentrations(se, averaged_assay, drc$row, drc$column, normalization_type, idfs)
  new_dt$row <- drc$row
  new_dt$column <- drc$column

  se <- gDRutils::persist_fit_assay(se, new_dt, merge = "merge", assay_name = metrics_assay,
                                    row = "row", col = "column",
                                    upsert_key_cols = c("fit_source", "normalization_type"))

  # PRISM fits the curves itself; the recipe re-imports them from the same table
  se <- gDRutils::record_fit_provenance(
    se, assay = metrics_assay, fit_source = "PRISM", fn = add_PRISM_metrics,
    fit_args = list(drc_path = drc_path, meta_data_path = meta_data_path, drug_map = drug_map,
                    normalization_type = normalization_type, metrics_assay = metrics_assay,
                    averaged_assay = averaged_assay)
  )
  gDRutils::record_processing_step(
    se, "prism_drc_import",
    settings = list(
      source = "PRISM DRC table",
      file = basename(drc_path),
      md5 = unname(tools::md5sum(drc_path)),
      screen = if ("screen" %in% names(drc)) unique(drc$screen),
      n_curves = n_curves,
      n_imported = sum(matched),
      n_left_out = sum(!matched)
    )
  )
}


# One Metrics row per PRISM curve. The curve parameters map onto the gDR model; PRISM's own
# AUC and IC50 keep their names, because their definitions differ from x_AOC and xc50.
#' @keywords internal
.prism_curves_to_metrics <- function(drc, normalization_type) {
  na <- rep(NA_real_, NROW(drc))
  fit_type <- as.character(drc$fit_name)
  fit_type[is.na(fit_type) | !nzchar(fit_type)] <- NA_character_
  out <- data.table::data.table(
    x_mean = na, x_AOC = na, x_AOC_range = na, xc50 = na, x_max = na,
    ec50 = as.numeric(drc$inflection),
    x_inf = as.numeric(drc$lower_limit),
    x_0 = as.numeric(drc$upper_limit),
    h = -as.numeric(drc$slope),
    r2 = as.numeric(drc$frac_var_explained),
    p_value = na, rss = na,
    maxlog10Concentration = log10(as.numeric(drc$maximum_dose)),
    x_sd_avg = na,
    fit_type = fit_type,
    normalization_type = normalization_type,
    fit_source = "PRISM"
  )
  prism_cols <- c(auc = "prism_auc", log2_auc = "prism_log2_auc", auc_riemann = "prism_auc_riemann",
                  ic50 = "prism_ic50", log2_ic50 = "prism_log2_ic50", mse = "prism_mse",
                  mad = "prism_mad", successful_fit = "prism_successful_fit",
                  minimum_dose = "prism_minimum_dose", maximum_dose = "prism_maximum_dose")
  for (col in intersect(names(prism_cols), names(drc))) out[[prism_cols[[col]]]] <- drc[[col]]
  # the curve identity within a gDR cell
  id_cols <- c("depmap_id", "pert_id", "pool_id", "lua", "cell_set", "pert_plate", "x_project_id", "screen")
  for (col in intersect(id_cols, names(drc))) {
    out[[col]] <- as.character(drc[[col]])
  }
  out
}


# Number of concentrations with a value in the averaged data of each (row, column) cell.
#' @keywords internal
.count_concentrations <- function(se, averaged_assay, rows, columns, normalization_type, idfs) {
  if (!averaged_assay %in% SummarizedExperiment::assayNames(se)) return(rep(NA_integer_, length(rows)))
  avg <- data.table::as.data.table(BumpyMatrix::unsplitAsDataFrame(
    SummarizedExperiment::assay(se, averaged_assay), row.field = "row", column.field = "column"
  ))
  conc <- idfs[["concentration"]]
  avg <- avg[avg$normalization_type == normalization_type & !is.na(avg$x) & avg[[conc]] > 0]
  counts <- avg[, list(N_conc = data.table::uniqueN(get(conc))), by = c("row", "column")]
  n <- counts$N_conc[match(paste(rows, columns), paste(counts$row, counts$column))]
  as.integer(n)
}


# The curve parameters of a PRISM DRC table.
#' @keywords internal
.prism_drc_fit_columns <- function() {
  c("lower_limit", "upper_limit", "slope", "inflection")
}
