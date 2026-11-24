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

  # identifiers read as text; depmap_id is optional when the table has ccle_name
  id_cols <- intersect(c("pert_name", "depmap_id"), names(data.table::fread(drc_path, nrows = 0L)))
  drc <- data.table::fread(drc_path, colClasses = list(character = id_cols))
  checkmate::assert_names(names(drc), must.include = c(
    .prism_drc_fit_columns(), "pert_name", "day", "fit_name", "frac_var_explained", "maximum_dose"
  ))

  .add_prism_cellline_duration(drc, meta_data_path)

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


#' Read PRISM's replicate-level log2 fold changes for a refit
#'
#' Reads PRISM's replicate-level LEVEL5 data (the \code{*_l2fc.csv} of an MTS or EPS delivery,
#' the file \code{\link{convert_LEVEL5_prism_to_gDR_input}} imports), with one row per
#' replicate point, to fit it with PRISM's algorithm, e.g. by \code{gDRcore::add_PRISM_refit_metrics()}.
#' Unlike the import, the log2 fold changes are kept as they are (no cap), and the columns that
#' tell PRISM's curves apart (pool, LUA, cell set, compound plate) are kept. The cell line
#' identifier and Duration are added as \code{\link{read_PRISM_drc}} adds them.
#'
#' The LEVEL5 column synonyms are accepted: \code{LFC_cb}, \code{LFC.cb} or \code{LFC} for
#' \code{l2fc}, \code{pert_iname} for \code{pert_name} and \code{pert_time} for \code{day}.
#'
#' @param l2fc_path string, path to the replicate-level PRISM data (CSV).
#' @param meta_data_path string, path to the DepMap \code{Model.csv} that maps \code{depmap_id}
#'   to the CCLE name. Not needed when the data have a \code{ccle_name} column.
#'
#' @return \code{data.table} with the columns of the file (\code{l2fc}, \code{pert_name},
#'   \code{pert_dose} and \code{day} under these names), the cell line identifier and
#'   \code{Duration} in hours. Rows with a missing dose or log2 fold change are left out. The input
#'   files are recorded in the attribute read by \code{\link{get_import_provenance}}.
#'
#' @examples
#' points <- read_PRISM_l2fc(
#'   l2fc_path = system.file("testdata/prism_l2fc.csv", package = "gDRimport"),
#'   meta_data_path = system.file("testdata/prism_drc_model.csv", package = "gDRimport")
#' )
#' head(points[, c("clid", "Duration", "lua", "pert_name", "pert_dose", "l2fc")])
#'
#' @keywords prism_conversion
#' @export
read_PRISM_l2fc <- function(l2fc_path, meta_data_path = NULL) {
  checkmate::assert_file_exists(l2fc_path)
  if (!is.null(meta_data_path)) checkmate::assert_file_exists(meta_data_path)

  cols <- names(data.table::fread(l2fc_path, nrows = 0L))
  text_cols <- intersect(c("pert_name", "pert_iname", "depmap_id", "ccle_name", "pool_id", "lua",
                           "cell_set", "pert_id", "pert_plate", "x_project_id"), cols)
  points <- data.table::fread(l2fc_path, colClasses = list(character = text_cols))
  synonyms <- .prism_level5_column_synonyms()
  renames <- c(l2fc = "LFC_cb", pert_name = "pert_iname", day = "pert_time")
  for (name in names(renames)) {
    found <- intersect(synonyms[[renames[[name]]]], names(points))
    if (!name %in% names(points) && length(found)) data.table::setnames(points, found[1], name)
  }
  checkmate::assert_names(names(points), must.include = c("l2fc", "pert_name", "pert_dose", "day"))
  points <- points[is.finite(points$pert_dose) & is.finite(points$l2fc)]

  .add_prism_cellline_duration(points, meta_data_path)
  data.table::setattr(points, "gDR_import", .import_record(
    files = list(
      .import_files("prism_l2fc", l2fc_path),
      .import_files("meta_data", meta_data_path)
    ),
    instrument = NULL,
    events = list(),
    source = list(format = "PRISM l2fc")
  ))
  points
}


# Adds the cell line identifier (the CCLE name, as convert_LEVEL5_prism_to_gDR_input() uses it,
# from ccle_name or from depmap_id through Model.csv; "" -> NA) and Duration (day * 24) to a PRISM
# table, by reference.
#' @keywords internal
.add_prism_cellline_duration <- function(dt, meta_data_path) {
  idfs <- gDRutils::get_env_identifiers()
  if ("ccle_name" %in% names(dt)) {
    cellline <- dt$ccle_name
  } else {
    checkmate::assert_names(names(dt), must.include = "depmap_id")
    if (is.null(meta_data_path)) {
      stop("'meta_data_path' is needed to map depmap_id to the cell line identifier")
    }
    meta <- data.table::fread(meta_data_path)
    checkmate::assert_names(names(meta), must.include = c("ModelID", "CCLEName"))
    cellline <- meta$CCLEName[match(dt$depmap_id, meta$ModelID)]
  }
  cellline[!is.na(cellline) & !nzchar(cellline)] <- NA_character_
  data.table::set(dt, j = idfs$cellline, value = cellline)
  data.table::set(dt, j = idfs$duration, value = as.numeric(dt$day) * 24)
  invisible(dt)
}
