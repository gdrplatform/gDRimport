####
# Import provenance
####
#
# load_data() records where the data came from: the files it read, with an md5 of
# their content, the reader that parsed the results, and the corrections gDRimport
# made on its own (renamed template sheets, empty cells filled with 0, untreated tags
# written in a different case). The record travels with the results table as the
# "gDR_import" attribute; gDRcore copies it into metadata(se)$provenance$steps$import.
#
# The corrections happen several calls below load_data(), so they are collected in an
# import log that is open only while one load_data() call runs.

.import_log_env <- new.env(parent = emptyenv())


#' Where gDR input data came from
#'
#' \code{\link{load_data}} and the PRISM converters attach a record of their
#' inputs to the table they return: the gDRimport version, the reader
#' (\code{instrument}), every input file with its role, size and md5, and the
#' corrections gDRimport applied on its own. gDRcore keeps the record in the
#' provenance of the processed object (\code{provenance$steps$import}).
#'
#' @param x the \code{data} element returned by \code{\link{load_data}}, or a
#'   table returned by a PRISM converter.
#'
#' @return The record (a list), or \code{NULL} when \code{x} carries none.
#'
#' @examples
#' td <- get_test_data()
#' l_data <- load_data(manifest_path(td), template_path(td), result_path(td))
#' get_import_provenance(l_data$data)$files
#'
#' @keywords load_files
#' @export
#'
get_import_provenance <- function(x) {
  attr(x, "gDR_import", exact = TRUE)
}


# Run `expr` with an empty import log; returns the value of `expr` and the logged
# events. The previous log is restored on exit, so nested loads do not mix.
#' @keywords internal
.with_import_log <- function(expr) {
  previous <- .import_log_env$current
  log <- new.env(parent = emptyenv())
  log$events <- list()
  .import_log_env$current <- log
  on.exit(.import_log_env$current <- previous, add = TRUE)
  value <- expr
  list(value = value, events = log$events)
}


# Add one event to the open import log; does nothing when no log is open (e.g. when
# a reader is called on its own).
#' @keywords internal
.note_import <- function(type, ...) {
  log <- .import_log_env$current
  if (is.null(log)) return(invisible(NULL))
  log$events[[length(log$events) + 1L]] <- list(type = type, ...)
  invisible(NULL)
}


# One row per input file: role, name, size and md5 of the content.
#' @keywords internal
.import_files <- function(role, path, name = basename(path)) {
  if (length(path) == 0L) return(NULL)
  data.frame(
    role = role,
    name = name,
    size = unname(file.size(path)),
    md5 = unname(tools::md5sum(path)),
    stringsAsFactors = FALSE
  )
}


# The record attached to the loaded data.
#' @keywords internal
.import_record <- function(files, instrument, events, source = NULL) {
  files <- do.call(rbind, files)
  rownames(files) <- NULL
  out <- list(
    gDRimport = as.character(utils::packageVersion("gDRimport")),
    source = source,
    instrument = instrument,
    files = files,
    corrections = .summarize_import_events(events)
  )
  out[!vapply(out, is.null, logical(1))]
}


# Corrections summed over the files and plates they happened in.
#' @keywords internal
.summarize_import_events <- function(events) {
  of_type <- function(type) Filter(function(e) identical(e$type, type), events)
  renamed <- of_type("template_sheet_renamed")
  filled <- of_type("cells_filled")
  untreated <- of_type("untreated_standardized")
  no_barcode <- of_type("plate_barcode_missing")
  out <- list(
    template_sheets_renamed = if (length(renamed) > 0L) {
      vapply(renamed, function(e) sprintf("%s: %s -> %s", e$file, e$from, e$to), character(1))
    },
    cells_filled_with_zero = if (length(filled) > 0L) sum(vapply(filled, `[[`, numeric(1), "n")),
    untreated_tags_standardized = if (length(untreated) > 0L) {
      sum(vapply(untreated, `[[`, numeric(1), "n"))
    },
    # one row per plate read without a barcode; `used` is the barcode the plate was
    # imported under (NA when the plate was skipped)
    plates_without_barcode = if (length(no_barcode) > 0L) {
      data.frame(
        file = vapply(no_barcode, `[[`, character(1), "file"),
        plate = vapply(no_barcode, `[[`, character(1), "plate"),
        header = vapply(no_barcode, `[[`, character(1), "header"),
        used = vapply(no_barcode, `[[`, character(1), "used"),
        stringsAsFactors = FALSE
      )
    }
  )
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0L) NULL else out
}
