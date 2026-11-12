#' Read the curve fits delivered by PRISM
#'
#' Reads a PRISM dose-response table (the \code{DRC_TABLE} of an MTS or EPS delivery), with
#' one row per curve PRISM fitted. The cell line identifier and Duration are added in the
#' form \code{\link{convert_LEVEL5_prism_to_gDR_input}} gives them, so the curves can be
#' matched to the processed data, e.g. by \code{gDRcore::add_PRISM_metrics()}, which adds them
#' to the \code{Metrics} assay as a second fit source.
#'
#' PRISM fits \code{upper_limit + (lower_limit - upper_limit) / (1 + (dose / inflection)^slope)};
#' the table is returned as delivered, without any change to these values.
#'
#' @param drc_path string, path to the PRISM DRC table (CSV).
#' @param meta_data_path string, path to the DepMap \code{Model.csv} that maps \code{depmap_id}
#'   to the CCLE name (the file given to \code{convert_LEVEL5_prism_to_gDR_input()}). Not needed
#'   when the table has a \code{ccle_name} column.
#'
#' @return \code{data.table} with the columns of the DRC table, the cell line identifier
#'   (the CCLE name, under the name of the \code{cellline} identifier) and \code{Duration}
#'   in hours (\code{day * 24}). Rows whose \code{depmap_id} has no CCLE name in
#'   \code{Model.csv} get \code{NA}, as they are left out of the LEVEL5 import. The input
#'   files are recorded in the attribute read by \code{\link{get_import_provenance}}.
#'
#' @examples
#' curves <- read_PRISM_drc(
#'   drc_path = system.file("testdata/prism_drc.csv", package = "gDRimport"),
#'   meta_data_path = system.file("testdata/prism_drc_model.csv", package = "gDRimport")
#' )
#' curves[, c("clid", "Duration", "pert_name", "lower_limit", "upper_limit", "slope", "inflection")]
#'
#' @keywords prism_conversion
#' @export
read_PRISM_drc <- function(drc_path, meta_data_path = NULL) {
  checkmate::assert_file_exists(drc_path)
  if (!is.null(meta_data_path)) checkmate::assert_file_exists(meta_data_path)

  drc <- data.table::fread(drc_path, colClasses = list(character = c("pert_name", "depmap_id")))
  checkmate::assert_names(names(drc), must.include = c(
    .prism_drc_fit_columns(), "pert_name", "day", "fit_name", "frac_var_explained", "maximum_dose"
  ))

  # cell line: the CCLE name, as convert_LEVEL5_prism_to_gDR_input() uses it
  idfs <- gDRutils::get_env_identifiers()
  if ("ccle_name" %in% names(drc)) {
    cellline <- drc$ccle_name
  } else {
    checkmate::assert_names(names(drc), must.include = "depmap_id")
    if (is.null(meta_data_path)) {
      stop("'meta_data_path' is needed to map depmap_id to the cell line identifier")
    }
    meta <- data.table::fread(meta_data_path)
    checkmate::assert_names(names(meta), must.include = c("ModelID", "CCLEName"))
    cellline <- meta$CCLEName[match(drc$depmap_id, meta$ModelID)]
  }
  cellline[!is.na(cellline) & !nzchar(cellline)] <- NA_character_
  drc[[idfs$cellline]] <- cellline
  drc[[idfs$duration]] <- as.numeric(drc$day) * 24

  data.table::setattr(drc, "gDR_import", .import_record(
    files = list(
      .import_files("prism_drc", drc_path),
      .import_files("meta_data", meta_data_path)
    ),
    instrument = NULL,
    events = list(),
    source = list(format = "PRISM DRC")
  ))
  drc
}


# The curve parameters of a PRISM DRC table.
#' @keywords internal
.prism_drc_fit_columns <- function() {
  c("lower_limit", "upper_limit", "slope", "inflection")
}
