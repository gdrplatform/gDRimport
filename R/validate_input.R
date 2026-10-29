####
# Pre-flight validation of gDR input data
####
#
# validate_gdr_input() looks at a set of input files before they are imported and
# reports every problem it finds at once, including the ones load_data() lets through
# without an error: wells dropped because a concentration is not a number, plates read
# without a barcode, duplicated wells, a day-0 plate labelled with the wrong Duration.
# It runs the package's own loaders, so a load_data failure reported here is the error
# the import would raise.
#
# Every check returns a table of findings; nothing is kept in global state. The input
# files are only read, never written.


#' Validate gDR input data before import
#'
#' Checks a set of input files and reports every problem it finds, from unreadable
#' files to plate-level anomalies in the readouts. Bench input (a manifest, treatment
#' templates and raw plate-reader files) is loaded with \code{\link{load_data}}, so a
#' failure reported for \code{load_data} is the error the import itself would raise.
#' The checks also flag problems the loaders let through without an error: values in a
#' Concentration sheet that are not numbers (those wells are dropped), raw plates
#' without a barcode, duplicated wells, plates missing from the manifest or from the
#' data, treated wells whose cell line and Duration have no control wells, drugs that
#' appear only at concentration 0, and Duration labels that do not fit the readouts of
#' the control wells. PRISM
#' input, processed MultiAssayExperiments and D300 files are recognised and checked
#' too.
#'
#' The input files are only read, never modified. With \code{fix = TRUE} they are copied
#' to \code{output_dir}, the mechanical problems (fix tier \code{"A"}) are repaired in
#' the copies, and the copies are validated again: files re-encoded as UTF-8, spaces
#' trimmed from file names, long tables saved as \code{.tsv}, \code{Gnumber_1} and
#' \code{Concentration_1} sheets renamed, \code{n/a} and \code{-99} cleared from
#' templates, and manifest references that only differ from a template file by spaces
#' or the extension rewritten. In xlsx files only the repaired cells change.
#'
#' @param paths character, paths to input files and/or directories. For a directory,
#'   the files directly inside it are used, not those in its subdirectories.
#' @param type character, the kind of input: \code{"auto"} (detected from the files),
#'   \code{"bench"} (manifest, templates and raw plate-reader files), \code{"prism"},
#'   \code{"mae"} (a processed gDR MultiAssayExperiment saved as \code{.qs2}) or
#'   \code{"pre_d300"} (D300 \code{.tdd} files that still need \code{\link{import_D300}}).
#' @param instrument character, the plate-reader format passed to
#'   \code{\link{load_data}}. When \code{NULL}, it is detected from the first raw file
#'   with \code{\link{detect_file_format}}.
#' @param id_patterns list of regular expressions for identifier formats.
#'   \code{drug} is the format of a drug identifier once its batch suffix is removed
#'   (the default is the one \code{gDRutils::remove_drug_batch} uses), and
#'   \code{cellline} the format of a cell line identifier (no default). An element set
#'   to \code{NULL} switches its check off; elements left out keep their default.
#' @param fix logical, whether to repair copies of the files in \code{output_dir}.
#' @param output_dir character, a new or empty directory for the repaired copies;
#'   required with \code{fix = TRUE}.
#'
#' @return An object of class \code{gdr_validation}, a list with
#' \describe{
#'   \item{verdict}{\code{"PASS"}, \code{"WARN"} or \code{"FAIL"}, the most severe
#'     finding (\code{"INFO"} counts as \code{"PASS"}).}
#'   \item{data_type}{the kind of input that was validated.}
#'   \item{files}{a \code{data.table} with one row per input file: \code{file},
#'     \code{path}, \code{role} and \code{format} (the plate-reader format of raw
#'     files).}
#'   \item{findings}{a \code{data.table} with one row per finding: \code{check},
#'     \code{severity} (\code{"PASS"}, \code{"INFO"}, \code{"WARN"} or \code{"FAIL"}),
#'     \code{detail}, \code{fix_tier} (\code{"A"} a mechanical fix, \code{"B"} a fix to
#'     confirm with whoever made the files, \code{"C"} a decision about the experiment,
#'     \code{NA} when there is nothing to fix) and \code{status_code}, the matching row
#'     of \code{\link{get_exception_data}} (\code{NA} when there is none).}
#'   \item{fixes}{character, one note per repair (empty without \code{fix}).}
#'   \item{output_dir}{the directory of the repaired copies, \code{NA} without
#'     \code{fix}. With \code{fix = TRUE}, \code{files} and \code{findings} describe
#'     the copies after the repairs.}
#' }
#'
#' @examples
#' td <- get_test_data()
#' res <- validate_gdr_input(c(manifest_path(td), template_path(td), result_path(td)))
#' res$verdict
#' res$findings
#'
#' # repair copies of the files instead of only reporting
#' fixed <- validate_gdr_input(c(manifest_path(td), template_path(td), result_path(td)),
#'                             fix = TRUE, output_dir = tempfile())
#' fixed$fixes
#'
#' @keywords correction_exception
#' @export
#'
validate_gdr_input <- function(paths,
                               type = c("auto", "bench", "prism", "mae", "pre_d300"),
                               instrument = NULL,
                               id_patterns = list(drug = "^G[0-9]{8}", cellline = NULL),
                               fix = FALSE,
                               output_dir = NULL) {
  checkmate::assert_character(paths, min.len = 1, any.missing = FALSE)
  type <- match.arg(type)
  checkmate::assert_choice(instrument, .supported_instruments(), null.ok = TRUE)
  checkmate::assert_list(id_patterns, types = c("character", "null"), names = "unique")
  checkmate::assert_subset(names(id_patterns), c("drug", "cellline"))
  id_patterns <- utils::modifyList(list(drug = "^G[0-9]{8}", cellline = NULL), id_patterns)
  checkmate::assert_flag(fix)
  checkmate::assert_string(output_dir, null.ok = TRUE)
  if (fix && is.null(output_dir)) {
    stop("'fix = TRUE' needs an 'output_dir' for the repaired copies; the input files are never changed.",
         call. = FALSE)
  }
  missing_paths <- paths[!file.exists(paths)]
  if (length(missing_paths) > 0L) {
    stop(sprintf("Path(s) not found: %s", toString(missing_paths)), call. = FALSE)
  }

  files <- .classify_input_files(.collect_input_files(paths))
  if (fix) return(.validate_and_fix(files, type, instrument, id_patterns, output_dir))
  data_type <- if (type == "auto") .detect_data_type(files) else type
  findings <- .safely(switch(
    data_type,
    bench = .validate_bench(files, instrument, id_patterns),
    prism = .validate_prism(files),
    mae = .validate_mae(files),
    pre_d300 = .validate_pre_d300(files),
    .finding("type", "FAIL", sprintf(
      "Could not tell what kind of gDR input the %d file(s) are; set 'type'.", NROW(files)
    ))
  ))
  .validation_result(data_type, files, findings)
}


#' @rdname validate_gdr_input
#' @param x a \code{gdr_validation} object.
#' @param ... not used.
#' @export
print.gdr_validation <- function(x, ...) {
  cat(sprintf("gDR input validation: %s, %d file(s)\n", x$data_type, NROW(x$files)))
  f <- x$findings
  for (i in seq_len(NROW(f))) {
    code <- if (is.na(f$status_code[i])) "" else sprintf(" [status %d]", f$status_code[i])
    cat(sprintf("  %-4s %-26s %s%s\n", f$severity[i], f$check[i], f$detail[i], code))
  }
  for (fix in x$fixes) cat(sprintf("  fixed: %s\n", fix))
  if (!is.na(x$output_dir)) cat(sprintf("Repaired copies: %s\n", x$output_dir))
  counts <- table(factor(f$severity, levels = rev(.severity_levels())))
  cat(sprintf("VERDICT: %s (%s)\n", x$verdict, toString(paste(counts, names(counts)))))
  invisible(x)
}


####
# Findings
####

# Severities from least to most severe; INFO does not change the verdict.
#' @keywords internal
.severity_levels <- function() {
  c("PASS", "INFO", "WARN", "FAIL")
}


# One finding as a one-row table (a typed empty table when `check` has length 0).
#' @keywords internal
.finding <- function(check, severity, detail, fix_tier = NA_character_, status_code = NA_integer_) {
  data.table::data.table(
    check = as.character(check),
    severity = as.character(severity),
    detail = as.character(detail),
    fix_tier = as.character(fix_tier),
    status_code = as.integer(status_code)
  )
}


# A finding worded by a row of get_exception_data(); `...` fills its %s. The bold
# markers meant for the shiny app are dropped.
#' @keywords internal
.coded_finding <- function(code, check, severity, fix_tier, ...) {
  text <- get_exception_data(code)$sprintf_text
  detail <- if (...length() > 0L) sprintf(text, ...) else text
  .finding(check, severity, gsub("**", "", detail, fixed = TRUE), fix_tier, code)
}


# Bind findings (one-row tables, or lists of them; NULLs are skipped).
#' @keywords internal
.bind_findings <- function(...) {
  parts <- Filter(Negate(is.null), list(...))
  parts <- lapply(parts, function(p) if (data.table::is.data.table(p)) p else do.call(.bind_findings, p))
  empty <- .finding(character(0), character(0), character(0), character(0), integer(0))
  data.table::rbindlist(c(list(empty), parts), use.names = TRUE)
}


# Run one check; an unexpected error becomes a FAIL instead of hiding the other checks.
# Warnings are dropped: what a check finds is reported as a finding, and text from
# broken input (invalid UTF-8) makes string functions warn.
#' @keywords internal
.safely <- function(expr) {
  tryCatch(
    withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning")),
    error = function(e) {
      .finding("internal", "FAIL", sprintf("A validation check failed: %s", conditionMessage(e)))
    }
  )
}


#' @keywords internal
.validation_result <- function(data_type, files, findings, fixes = character(0)) {
  findings <- .bind_findings(findings)
  worst <- max(c(1L, match(findings$severity, .severity_levels())), na.rm = TRUE)
  structure(
    list(
      verdict = c("PASS", "PASS", "WARN", "FAIL")[worst],
      data_type = data_type,
      files = files,
      findings = findings,
      fixes = fixes,
      output_dir = NA_character_
    ),
    class = c("gdr_validation", "list")
  )
}


####
# Helpers
####

# Evaluate `expr` with the loaders' logging, messages and warnings kept off the console.
# Returns list(value, error): on failure `value` is NULL and `error` the condition
# message. The futile.logger threshold is restored on exit.
#' @keywords internal
.quietly <- function(expr) {
  threshold <- futile.logger::flog.threshold()
  futile.logger::flog.threshold(futile.logger::FATAL)
  on.exit(futile.logger::flog.threshold(threshold), add = TRUE)
  tryCatch(
    list(
      value = withCallingHandlers(
        expr,
        warning = function(w) invokeRestart("muffleWarning"),
        message = function(m) invokeRestart("muffleMessage")
      ),
      error = NULL
    ),
    error = function(e) list(value = NULL, error = conditionMessage(e))
  )
}


#' @keywords internal
.file_ext <- function(path) {
  tolower(tools::file_ext(path))
}


# The instruments load_results() accepts.
#' @keywords internal
.supported_instruments <- function() {
  c("EnVision", "EnVision_new", "Tecan", "Incucyte", "long_tsv")
}


# The first `n` values as one string, with a count of the rest.
#' @keywords internal
.head_list <- function(x, n = 8L) {
  x <- unique(x)
  out <- toString(utils::head(x, n))
  if (length(x) > n) out <- sprintf("%s and %d more", out, length(x) - n)
  out
}


# Column names of a table file (header row only); character(0) when it cannot be read.
#' @keywords internal
.peek_columns <- function(path) {
  cols <- .quietly(
    if (.file_ext(path) %in% c("xlsx", "xls")) {
      names(readxl::read_excel(path, n_max = 0))
    } else {
      names(data.table::fread(path, nrows = 0))
    }
  )$value
  if (is.null(cols)) character(0) else cols
}


# A pattern matching a sheet (or column) name for the given identifiers, ignoring case
# and a numeric suffix: c("drug", "concentration") -> "^(gnumber|concentration)(_[0-9]+)?$".
#' @keywords internal
.identifier_pattern <- function(keys) {
  ids <- tolower(unlist(gDRutils::get_env_identifiers(keys, simplify = FALSE), use.names = FALSE))
  sprintf("^(%s)(_[0-9]+)?$", paste(ids, collapse = "|"))
}


####
# Input files
####

# The files to validate: the paths given, with each directory replaced by the files
# directly inside it. Lock files, Office temp files, .DS_Store and .bak backups are left
# out.
#' @keywords internal
.collect_input_files <- function(paths) {
  files <- unlist(lapply(paths, function(p) {
    if (dir.exists(p)) {
      inside <- list.files(p, full.names = TRUE)
      inside[!dir.exists(inside)]
    } else {
      p
    }
  }), use.names = FALSE)
  files <- unique(normalizePath(files))
  files[!grepl("^~\\$|^\\.~lock|^\\.DS_Store$|\\.bak$", basename(files))]
}


# One row per file with its role: manifest, template, template_long, raw, raw_long,
# prism_data, prism_meta, prism_cell_lines, prism_treatment, mae, d300 or unknown.
# Manifests are found first, so the templates they reference are recognised by name
# even when they cannot be loaded on their own (a template that holds only the
# untreated wells, or one whose sheets are misnamed).
#' @keywords internal
.classify_input_files <- function(files) {
  out <- data.table::data.table(
    file = basename(files),
    path = files,
    role = rep("unknown", length(files)),
    format = rep(NA_character_, length(files))
  )
  if (length(files) == 0L) return(out)

  ext <- .file_ext(files)
  out$role[ext == "qs2"] <- "mae"
  out$role[ext == "tdd"] <- "d300"
  tabular <- which(out$role == "unknown" & ext %in% c("xlsx", "xls", "csv", "tsv", "txt"))
  for (i in tabular) {
    if (.is_manifest(files[i])) out$role[i] <- "manifest"
  }
  referenced <- .referenced_templates(files[out$role == "manifest"])
  for (i in tabular[out$role[tabular] == "unknown"]) {
    cls <- .classify_table_file(files[i], referenced)
    out$role[i] <- cls[["role"]]
    out$format[i] <- cls[["format"]]
  }
  out
}


# A manifest loads with load_manifest(); one that fails to load (a typo in a header)
# is still a manifest if it has a barcode-like and a template-like column, so the load
# error can be reported against it.
#' @keywords internal
.is_manifest <- function(path) {
  if (!is.null(.quietly(load_manifest(path))$value)) return(TRUE)
  cols <- tolower(gsub("[^A-Za-z0-9]", "", .peek_columns(path)))
  any(grepl("barcode|plateid|^plate$", cols)) && any(grepl("template|treatment", cols))
}


#' @keywords internal
.referenced_templates <- function(manifests) {
  unlist(lapply(manifests, function(path) {
    m <- .quietly(load_manifest(path))$value
    if (is.null(m)) return(character(0))
    trimws(as.character(m$data[[m$headers[["template"]]]]))
  }), use.names = FALSE)
}


# "raw" for a table with one row per well readout, "treatment" for one with one row
# per well treatment, NA otherwise.
#' @keywords internal
.long_table_kind <- function(path) {
  cols <- tolower(.peek_columns(path))
  if (!all(tolower(gDRutils::get_env_identifiers("well_position")) %in% cols)) return(NA_character_)
  if ("readoutvalue" %in% cols) return("raw")
  treatment_cols <- tolower(unlist(gDRutils::get_env_identifiers(c("drug", "drug_name", "concentration"),
                                                                 simplify = FALSE)))
  if (any(treatment_cols %in% cols)) return("treatment")
  NA_character_
}


#' @keywords internal
.has_template_sheets <- function(path) {
  sheets <- .quietly(readxl::excel_sheets(path))$value
  any(grepl(.identifier_pattern(c("drug", "concentration")), tolower(sheets)))
}


#' @keywords internal
.classify_table_file <- function(path, referenced) {
  ext <- .file_ext(path)
  is_xlsx <- ext %in% c("xlsx", "xls")
  kind <- .long_table_kind(path)
  if (trimws(basename(path)) %in% referenced || (is_xlsx && .has_template_sheets(path))) {
    # load_templates() reads long treatment tables only from tab-separated files
    role <- if (identical(kind, "treatment") && ext != "tsv") "template_long" else "template"
    return(list(role = role, format = NA_character_))
  }
  if (!is_xlsx) {
    prism <- .prism_role(.peek_columns(path))
    if (!is.na(prism)) return(list(role = prism, format = NA_character_))
  }
  if (identical(kind, "treatment")) {
    return(list(role = if (ext == "tsv") "template" else "template_long", format = NA_character_))
  }
  if (identical(kind, "raw")) {
    if (is_xlsx) return(list(role = "raw_long", format = NA_character_))
    return(list(role = "raw", format = "long_tsv"))
  }
  fmt <- .quietly(detect_file_format(path))$value
  if (!is.null(fmt) && fmt %in% c("EnVision", "EnVision_new", "Tecan")) {
    return(list(role = "raw", format = fmt))
  }
  list(role = "unknown", format = NA_character_)
}


#' @keywords internal
.detect_data_type <- function(files) {
  roles <- files$role
  bench_roles <- c("manifest", "template", "template_long", "raw", "raw_long")
  # a .qs2 next to bench files (a reference or results object) is not the input
  if (any(roles == "mae") && !any(roles %in% bench_roles)) return("mae")
  if (any(startsWith(roles, "prism_"))) return("prism")
  if (any(roles == "d300") && !any(roles %in% c("template", "template_long"))) return("pre_d300")
  if (any(roles %in% bench_roles)) return("bench")
  "unknown"
}


# Problems with the set of files itself: missing roles, unreadable or empty files,
# names that break shell globs, raw files from more than one instrument.
#' @keywords internal
.check_input_files <- function(files) {
  role <- files$role
  out <- list()
  if (!any(role == "manifest")) {
    out <- c(out, list(.finding("files", "FAIL", "No manifest file was found.", "B", 2L)))
  }
  if (!any(role %in% c("template", "template_long"))) {
    out <- c(out, list(.finding("files", "FAIL", "No treatment template file was found.", "B", 11L)))
  }
  if (!any(role %in% c("raw", "raw_long"))) {
    out <- c(out, list(.coded_finding(43L, "files", "FAIL", "C")))
  }
  unknown <- files$file[role == "unknown"]
  if (length(unknown) > 0L) {
    out <- c(out, list(.finding("files", "INFO", sprintf(
      "Not recognised as gDR input, so not used: %s", .head_list(unknown)
    ))))
  }
  # e.g. a reference or results .qs2 saved next to the bench files
  other <- files$file[!role %in% c("unknown", "manifest", "template", "template_long", "raw", "raw_long")]
  if (length(other) > 0L) {
    out <- c(out, list(.finding("files", "INFO", sprintf(
      "Not bench input, so not used here: %s", .head_list(other)
    ))))
  }
  numbers <- unknown[.file_ext(unknown) == "numbers"]
  if (length(numbers) > 0L) {
    out <- c(out, list(.finding("file_format", "WARN", sprintf(
      "Apple Numbers file(s) cannot be read; export them as .xlsx: %s", .head_list(numbers)
    ), "B", 13L)))
  }
  empty <- files$file[file.size(files$path) == 0]
  if (length(empty) > 0L) {
    out <- c(out, list(.coded_finding(41L, "empty_files", "FAIL", "C", .head_list(empty))))
  }
  spaced <- files$file[grepl("^\\s|\\s$|\\s\\.[A-Za-z0-9]+$", files$file)]
  if (length(spaced) > 0L) {
    out <- c(out, list(.finding("filename_whitespace", "INFO", sprintf(
      "File name(s) start or end with a space, which breaks file patterns on the command line: %s",
      .head_list(sprintf("'%s'", spaced))
    ), "A")))
  }
  formats <- unique(stats::na.omit(files$format[role == "raw"]))
  if (length(formats) > 1L) {
    out <- c(out, list(.finding("mixed_instruments", "WARN", sprintf(
      "Raw files come in more than one plate-reader format (%s); the import reads all of them as one.",
      toString(formats)
    ), "C")))
  }
  .bind_findings(out)
}


# Text inputs that are not valid UTF-8 (e.g. a Windows-1252 export with a "mu" sign).
# Only the first 25 MB of a file are read, which is enough to catch an export encoding.
#' @keywords internal
.check_encoding <- function(files) {
  text <- files$path[.file_ext(files$path) %in% c("csv", "tsv", "txt")]
  bad <- text[!vapply(text, .is_valid_utf8, logical(1))]
  if (length(bad) == 0L) return(NULL)
  .coded_finding(40L, "encoding", "FAIL", "A", .head_list(basename(bad)))
}


#' @keywords internal
.is_valid_utf8 <- function(path) {
  bytes <- readBin(path, "raw", n = min(file.size(path), 25e6))
  text <- tryCatch(rawToChar(bytes), error = function(e) NA_character_)
  # a file rawToChar() cannot read (embedded nul) is not an encoding problem
  is.na(text) || validUTF8(text)
}


####
# Bench input: manifest, templates, raw plate-reader files
####

#' @keywords internal
.validate_bench <- function(files, instrument, id_patterns) {
  path_of <- function(roles) files$path[files$role %in% roles]
  manifest <- path_of("manifest")
  templates <- path_of("template")
  raw <- path_of("raw")
  out <- list(.safely(.check_input_files(files)), .safely(.check_encoding(files)))

  long_templates <- basename(path_of("template_long"))
  if (length(long_templates) > 0L) {
    out <- c(out, list(.coded_finding(42L, "long_format_treatment", "FAIL", "A", .head_list(long_templates))))
  }
  long_raw <- basename(path_of("raw_long"))
  if (length(long_raw) > 0L) {
    out <- c(out, list(.coded_finding(42L, "long_format_raw_xlsx", "FAIL", "A", .head_list(long_raw))))
  }
  if (length(manifest) == 0L || length(templates) == 0L || length(raw) == 0L) {
    return(.bind_findings(out))
  }

  manifest_check <- .check_manifest(manifest)
  m <- manifest_check$manifest
  # a template that cannot be read is reported by load_data below
  cache <- tryCatch(.read_templates(templates), error = function(e) list())
  out <- c(out, list(
    manifest_check$findings,
    if (!is.null(m)) .safely(.check_template_references(m, templates)),
    .safely(.check_sheet_naming(cache)),
    .safely(.check_border_placeholders(cache)),
    .safely(.check_concentrations(cache)),
    .safely(.check_combo_gradient(cache)),
    .safely(.check_drug_ids(cache, id_patterns[["drug"]])),
    .safely(.check_cellline_ids(m, cache, id_patterns[["cellline"]]))
  ))

  if (is.null(instrument)) {
    instrument <- files$format[files$role == "raw"][1]
    if (is.na(instrument)) instrument <- "EnVision"
  }
  load_check <- .check_load_data(manifest, templates, raw, instrument)
  out <- c(out, list(load_check$findings))
  loaded <- load_check$loaded
  if (is.null(loaded)) return(.bind_findings(out))

  headers <- m$headers
  provenance <- get_import_provenance(loaded$data)
  manifest_barcodes <- as.character(loaded$manifest[[headers[["barcode"]]]])
  data_barcodes <- as.character(loaded$data[[headers[["barcode"]]]])
  out <- c(out, list(
    .safely(.check_import_corrections(provenance)),
    .safely(.check_plate_barcodes(provenance, data_barcodes, manifest_barcodes)),
    .safely(.check_duplicate_wells(loaded$data, headers)),
    .safely(.check_barcode_coverage(manifest_barcodes, data_barcodes))
  ))

  # wells joined to their manifest row and treatment; a finding when the join fails
  view <- .safely({
    wells <- .well_table(loaded, headers)
    list(wells = wells, plates = .plate_table(wells, headers))
  })
  if (data.table::is.data.table(view)) return(.bind_findings(out, view))
  .bind_findings(out, list(
    .safely(.check_control_wells(view$wells, headers)),
    .safely(.check_zero_dose_drugs(view$wells, headers)),
    .safely(.check_readouts(view$wells, view$plates)),
    .safely(.check_plate_outliers(view$plates)),
    .safely(.check_duration_plausibility(view$plates))
  ))
}


# Load the manifest(s) and check Duration; returns the findings and the loaded manifest
# (NULL when it does not load).
#' @keywords internal
.check_manifest <- function(manifest) {
  res <- .quietly(load_manifest(manifest))
  if (!is.null(res$error)) {
    code <- .exception_code_from_message(res$error)
    if (is.na(code)) code <- 1L
    return(list(findings = .finding("manifest", "FAIL", res$error, "C", code), manifest = NULL))
  }
  m <- res$value
  out <- list(.finding("manifest", "PASS", sprintf("Manifest loaded (%d plate(s)).", NROW(m$data))))
  duration_col <- m$headers[["duration"]]
  if (!is.null(duration_col) && duration_col %in% names(m$data)) {
    duration <- suppressWarnings(as.numeric(m$data[[duration_col]]))
    if (any(is.na(duration))) {
      out <- c(out, list(.finding("duration", "WARN", sprintf(
        "%d manifest row(s) have a %s that is not a number.", sum(is.na(duration)), duration_col
      ), "C")))
    } else if (any(duration < 0)) {
      out <- c(out, list(.finding("duration", "WARN", sprintf(
        "%d manifest row(s) have a negative %s.", sum(duration < 0), duration_col
      ), "C")))
    }
  }
  list(findings = .bind_findings(out), manifest = m)
}


# Templates named in the manifest that are not among the files. When the name only
# differs by spaces or a missing extension, fixing the manifest is mechanical.
#' @keywords internal
.check_template_references <- function(m, templates) {
  referenced <- unique(as.character(m$data[[m$headers[["template"]]]]))
  referenced <- referenced[!is.na(referenced) & nzchar(referenced)]
  present <- basename(templates)
  missing <- setdiff(referenced, present)
  if (length(missing) == 0L) return(NULL)
  # compare the names without spaces around them or their extension
  bare <- function(x) trimws(tools::file_path_sans_ext(trimws(x)))
  resolvable <- bare(missing) %in% bare(present)
  .coded_finding(3L, "templates", "WARN", if (all(resolvable)) "A" else "C", .head_list(missing, 12L))
}


####
# Treatment templates
####

# Read every template once, the way load_templates() does, so the sheet checks see the
# cells the import reads. xlsx sheets are read as text from A1 over the plate range the
# loader uses, keyed by the name the loader gives them (correct_template_sheets() turns
# e.g. "gnumber" into "Gnumber"); tsv templates are read as tables, keyed by column.
#' @keywords internal
.read_templates <- function(templates) {
  xlsx <- templates[.file_ext(templates) %in% c("xlsx", "xls")]
  tsv <- setdiff(templates, xlsx)
  names_used <- .corrected_sheet_names(xlsx)
  wp <- gDRutils::get_env_identifiers("well_position")
  from_xlsx <- lapply(seq_along(xlsx), function(i) {
    physical <- .quietly(readxl::excel_sheets(xlsx[i]))$value
    used <- names_used[[i]]
    if (length(used) != length(physical)) used <- physical
    drug_idx <- which(tolower(used) == tolower(gDRutils::get_env_identifiers("drug")))[1]
    plate <- if (!is.na(drug_idx)) .quietly(get_plate_info_from_template_xlsx(xlsx[i], drug_idx, 1L))$value
    range <- if (is.null(plate)) "A1:AV32" else plate$plate_range
    sheets <- lapply(seq_along(physical), function(s) {
      cells <- .quietly(read_excel_to_dt(xlsx[i], sheet = s, range = range, col_names = FALSE,
                                         col_types = "text", .name_repair = "minimal"))$value
      if (is.null(cells)) NULL else as.matrix(cells)
    })
    names(sheets) <- used
    list(path = xlsx[i], physical = physical, sheets = sheets)
  })
  from_tsv <- lapply(tsv, function(path) {
    cells <- .quietly(data.table::fread(path, sep = "\t", colClasses = "character"))$value
    sheets <- if (is.null(cells)) list() else as.list(cells)[setdiff(names(cells), wp)]
    list(path = path, physical = names(sheets), sheets = sheets)
  })
  c(from_xlsx, from_tsv)
}


# Sheet names after correct_template_sheets(), tried on the whole set first (a template
# holding only untreated wells is valid only next to the others), then file by file;
# the file's own sheet names when neither works.
#' @keywords internal
.corrected_sheet_names <- function(xlsx) {
  if (length(xlsx) == 0L) return(list())
  all_fixed <- .quietly(correct_template_sheets(xlsx))$value
  if (!is.null(all_fixed)) return(unname(all_fixed))
  lapply(xlsx, function(path) {
    fixed <- .quietly(correct_template_sheets(path))$value
    if (is.null(fixed)) .quietly(readxl::excel_sheets(path))$value else fixed[[1]]
  })
}


# The non-empty cell values of the sheets whose name matches `pattern`, per template.
#' @keywords internal
.sheet_values <- function(template, pattern) {
  hit <- grepl(pattern, tolower(names(template$sheets)))
  lapply(template$sheets[hit], function(cells) {
    v <- trimws(as.character(cells))
    v[!is.na(v) & nzchar(v)]
  })
}


#' @keywords internal
.is_placeholder_cell <- function(v) {
  v <- trimws(as.character(v))
  !is.na(v) & (grepl("^n\\s*/\\s*a$", v, ignore.case = TRUE) | v == "-99")
}


# Sheets named Gnumber_1 / Concentration_1: load_templates() does not rename those, so
# the import fails on them.
#' @keywords internal
.check_sheet_naming <- function(cache) {
  pattern <- sprintf("^(%s)_1$", paste(tolower(unlist(gDRutils::get_env_identifiers(
    c("drug", "concentration"), simplify = FALSE))), collapse = "|"))
  bad <- vapply(cache, function(t) any(grepl(pattern, tolower(t$physical))), logical(1))
  if (!any(bad)) return(NULL)
  .finding("sheet_naming", "WARN", sprintf(
    "Sheets for the first drug must be named '%s' and '%s', without '_1': %s",
    gDRutils::get_env_identifiers("drug"), gDRutils::get_env_identifiers("concentration"),
    .head_list(basename(vapply(cache[bad], `[[`, character(1), "path")))
  ), "A", 25L)
}


# 'n/a' and '-99' where the empty-border convention needs empty cells; they leak into
# the drug and concentration annotation.
#' @keywords internal
.check_border_placeholders <- function(cache) {
  counts <- vapply(cache, function(t) {
    sum(vapply(t$sheets, function(cells) sum(.is_placeholder_cell(cells)), numeric(1)))
  }, numeric(1))
  if (!any(counts > 0)) return(NULL)
  hits <- sprintf("%s (%d)", basename(vapply(cache, `[[`, character(1), "path")), counts)[counts > 0]
  .finding("border_placeholder", "WARN", sprintf(
    "Template(s) use 'n/a' or '-99' where the plate border should be empty: %s", .head_list(hits)
  ), "A")
}


# Values in a Concentration sheet that are not numbers: the loader reads them as NA and
# the well is dropped without an error. Placeholders are left to the border check.
#' @keywords internal
.check_concentrations <- function(cache) {
  pattern <- .identifier_pattern(c("concentration", "concentration2", "concentration3"))
  bad <- unlist(lapply(cache, function(t) {
    per_sheet <- lapply(.sheet_values(t, pattern), function(v) {
      v <- v[!.is_placeholder_cell(v)]
      unique(v[is.na(suppressWarnings(as.numeric(v)))])
    })
    per_sheet <- per_sheet[lengths(per_sheet) > 0L]
    if (length(per_sheet) == 0L) return(NULL)
    sprintf("%s (%s)", basename(t$path), paste(
      sprintf("%s: %s", names(per_sheet), vapply(per_sheet, .head_list, character(1), n = 4L)),
      collapse = "; "
    ))
  }))
  if (length(bad) == 0L) return(NULL)
  .coded_finding(44L, "concentration_not_numeric", "FAIL", "C", .head_list(bad))
}


# A second drug held at one dose. gDR still builds the combination experiment, but the
# synergy surface has no Drug 2 gradient; a valid design for a fixed co-treatment, and
# also what a flattened Concentration_2 sheet looks like.
#' @keywords internal
.check_combo_gradient <- function(cache) {
  pattern <- sprintf("^%s$", tolower(gDRutils::get_env_identifiers("concentration2")))
  flat <- unlist(lapply(cache, function(t) {
    v <- suppressWarnings(as.numeric(unlist(.sheet_values(t, pattern))))
    # no non-zero dose means there is no second drug (e.g. an untreated template)
    if (length(unique(v[!is.na(v) & v > 0])) == 1L) basename(t$path)
  }))
  if (length(flat) == 0L) return(NULL)
  .finding("combo_degenerate", "INFO", sprintf(
    "Template(s) hold the second drug at a single dose, so the combination has no Drug 2 gradient: %s",
    .head_list(flat)
  ), "C")
}


# Drug identifiers that look like an attempt at `pattern` (they match it once the
# separators are removed) but do not reduce to it after gDRutils::remove_drug_batch(),
# e.g. "G-03406036.1-1". Such ids lose their drug annotation without an error.
#' @keywords internal
.check_drug_ids <- function(cache, pattern) {
  if (is.null(pattern)) return(NULL)
  ids <- unique(unlist(lapply(cache, function(t) {
    unlist(.sheet_values(t, .identifier_pattern(c("drug", "drug2", "drug3"))))
  })))
  attempts <- ids[grepl(pattern, gsub("[^A-Za-z0-9]", "", ids))]
  bad <- attempts[!grepl(paste0(pattern, "$"), gDRutils::remove_drug_batch(attempts, drug_p = pattern))]
  if (length(bad) == 0L) return(NULL)
  .finding("gnumber_format", "WARN", sprintf(
    paste0("Drug id(s) do not reduce to the expected format after removing the batch suffix, ",
           "so they get no drug annotation: %s"),
    .head_list(sprintf("'%s'", bad))
  ), "C")
}


# Cell line identifiers with stray whitespace: non-breaking or other unusual spaces
# anywhere, leading or trailing spaces, and (with a `pattern`) a space inside an
# identifier that matches the pattern once the space is removed ("CL 586181"). Each
# variant is read as a separate cell line.
#' @keywords internal
.check_cellline_ids <- function(m, cache, pattern) {
  cellline <- gDRutils::get_env_identifiers("cellline")
  ids <- if (!is.null(m) && cellline %in% names(m$data)) as.character(m$data[[cellline]])
  ids <- c(ids, unlist(lapply(cache, function(t) {
    hit <- tolower(names(t$sheets)) == tolower(cellline)
    unlist(lapply(t$sheets[hit], as.character))
  })))
  # text that is not valid UTF-8 is reported by the encoding check
  ids <- unique(ids[!is.na(ids) & nzchar(ids) & validUTF8(ids)])
  odd_space <- grepl("[\u00a0\u2000-\u200b\u202f\u3000\ufeff]", ids)
  bad <- ids[odd_space | ids != trimws(ids)]
  if (!is.null(pattern)) {
    inner <- grepl("\\s", trimws(ids)) & grepl(pattern, gsub("\\s", "", ids))
    bad <- unique(c(bad, ids[inner]))
  }
  if (length(bad) == 0L) return(NULL)
  .finding("clid_format", "WARN", sprintf(
    "Cell line id(s) contain stray whitespace, so each variant is read as a separate cell line: %s",
    .head_list(sprintf("'%s'", bad))
  ), "C")
}


####
# Loading and the loaded data
####

# Run load_data(); returns its findings and the loaded data (NULL when it fails).
#' @keywords internal
.check_load_data <- function(manifest, templates, raw, instrument) {
  res <- .quietly(load_data(manifest, templates, raw, instrument = instrument))
  if (!is.null(res$error)) {
    hint <- if (grepl("invalid multibyte|input string .* invalid|invalid UTF", res$error)) {
      " A file probably contains characters that are not valid UTF-8."
    } else {
      ""
    }
    return(list(
      findings = .finding("load_data", "FAIL", paste0("load_data() failed: ", res$error, hint), "C",
                          .exception_code_from_message(res$error)),
      loaded = NULL
    ))
  }
  list(
    findings = .finding("load_data", "PASS", sprintf(
      "load_data() read %d readout(s) as %s.", NROW(res$value$data), instrument
    )),
    loaded = res$value
  )
}


# Corrections load_data() made on its own, from the import record.
#' @keywords internal
.check_import_corrections <- function(provenance) {
  corrections <- provenance$corrections
  out <- list()
  if (!is.null(corrections$template_sheets_renamed)) {
    out <- c(out, list(.finding("import_corrections", "INFO", sprintf(
      "load_data() renamed template sheet(s): %s", .head_list(corrections$template_sheets_renamed)
    ))))
  }
  if (!is.null(corrections$untreated_tags_standardized)) {
    out <- c(out, list(.finding("import_corrections", "INFO", sprintf(
      "load_data() changed the case of %d untreated tag(s).", corrections$untreated_tags_standardized
    ))))
  }
  if (!is.null(corrections$cells_filled_with_zero)) {
    out <- c(out, list(.finding("import_corrections", "WARN", sprintf(
      "load_data() filled %d empty raw-data cell(s) with 0; check that those wells were really empty.",
      corrections$cells_filled_with_zero
    ), "C")))
  }
  .bind_findings(out)
}


# Plates read without a barcode. The import record names the plate when the reader
# knows it (EnVision); otherwise placeholder barcodes in the data are reported. A plate
# whose barcode came from the Background section and is in the manifest was read
# correctly, so it is only noted.
#' @keywords internal
.check_plate_barcodes <- function(provenance, data_barcodes, manifest_barcodes) {
  missing <- provenance$corrections$plates_without_barcode
  if (is.null(missing)) {
    placeholders <- unique(data_barcodes[.is_missing_barcode(data_barcodes)])
    if (length(placeholders) == 0L) return(NULL)
    return(.coded_finding(45L, "plate_barcode", "FAIL", "C", sprintf(
      "the data contains barcode(s) %s", .head_list(sprintf("'%s'", placeholders))
    )))
  }
  recovered <- !.is_missing_barcode(missing$used) & missing$used %in% manifest_barcodes
  out <- list()
  if (any(recovered)) {
    out <- c(out, list(.finding("plate_barcode", "INFO", sprintf(
      "Plate(s) without a barcode in the plate header were read with the barcode from the Background section: %s",
      .head_list(sprintf("%s plate %s -> '%s'", missing$file, missing$plate, missing$used)[recovered])
    ))))
  }
  if (any(!recovered)) {
    bad <- missing[!recovered, , drop = FALSE]
    outcome <- ifelse(is.na(bad$used), "skipped", sprintf("imported as '%s'", bad$used))
    out <- c(out, list(.coded_finding(45L, "plate_barcode", "FAIL", "C",
                                      .head_list(sprintf("%s plate %s (%s)", bad$file, bad$plate, outcome)))))
  }
  .bind_findings(out)
}


# Readouts that share a barcode and well with another readout of a different value
# (load_results() already drops exact repeats).
#' @keywords internal
.check_duplicate_wells <- function(data, headers) {
  keys <- c(headers[["barcode"]], gDRutils::get_env_identifiers("well_position"))
  if (!all(keys %in% names(data))) return(NULL)
  key <- do.call(paste, c(lapply(keys, function(k) as.character(data[[k]])), sep = " "))
  dup <- duplicated(key)
  if (!any(dup)) return(.finding("duplicate_wells", "PASS", "No barcode and well is read twice."))
  .coded_finding(46L, "duplicate_wells", "FAIL", "C", sum(dup), .head_list(key[dup], 5L))
}


#' @keywords internal
.check_barcode_coverage <- function(manifest_barcodes, data_barcodes) {
  only_manifest <- setdiff(manifest_barcodes, data_barcodes)
  only_data <- setdiff(data_barcodes, manifest_barcodes)
  if (length(only_manifest) == 0L && length(only_data) == 0L) {
    return(.finding("barcode_coverage", "PASS", "Every plate in the manifest has data, and the other way round."))
  }
  .bind_findings(
    if (length(only_manifest) > 0L) {
      .coded_finding(6L, "barcode_coverage", "WARN", "C", .head_list(only_manifest, 12L))
    },
    if (length(only_data) > 0L) {
      .coded_finding(7L, "barcode_coverage", "WARN", "C", .head_list(only_data, 12L))
    }
  )
}


# One row per raw readout, joined to its manifest row (template, Duration, cell line)
# and to the treatment of its well.
#' @keywords internal
.well_table <- function(loaded, headers) {
  wp <- gDRutils::get_env_identifiers("well_position")
  barcode <- headers[["barcode"]]
  template <- headers[["template"]]
  wells <- data.table::copy(loaded$data)
  wells[, (wp) := lapply(.SD, as.character), .SDcols = wp]
  manifest_cols <- intersect(c(barcode, template, headers[["duration"]], headers[["cellline"]]),
                             names(loaded$manifest))
  wells <- merge(wells, unique(loaded$manifest[, manifest_cols, with = FALSE]),
                 by = barcode, all.x = TRUE, sort = FALSE)

  treatments <- loaded$treatments
  drug_cols <- intersect(unlist(gDRutils::get_env_identifiers(c("drug", "drug2", "drug3"), simplify = FALSE)),
                         names(treatments))
  conc_cols <- intersect(unlist(gDRutils::get_env_identifiers(
    c("concentration", "concentration2", "concentration3"), simplify = FALSE)), names(treatments))
  masked <- intersect(gDRutils::get_env_identifiers("masked_tag"), names(treatments))
  if (template %in% names(wells) && all(c("Template", wp) %in% names(treatments))) {
    treatments <- unique(treatments[, c("Template", wp, drug_cols, conc_cols, masked), with = FALSE],
                         by = c("Template", wp))
    treatments[, (wp) := lapply(.SD, as.character), .SDcols = wp]
    wells <- merge(wells, treatments, by.x = c(template, wp), by.y = c("Template", wp),
                   all.x = TRUE, sort = FALSE)
  }
  wells
}


# Control wells: the first drug is an untreated tag, any other drug is empty or
# untreated, and the well is not masked.
#' @keywords internal
.is_control_well <- function(wells) {
  untreated <- tolower(gDRutils::get_env_identifiers("untreated_tag"))
  drug_cols <- intersect(unlist(gDRutils::get_env_identifiers(c("drug", "drug2", "drug3"), simplify = FALSE)),
                         names(wells))
  if (length(drug_cols) == 0L) return(rep(FALSE, NROW(wells)))
  control <- tolower(wells[[drug_cols[1]]]) %in% untreated
  for (col in drug_cols[-1]) {
    v <- tolower(wells[[col]])
    control <- control & (is.na(v) | v %in% untreated)
  }
  masked <- intersect(gDRutils::get_env_identifiers("masked_tag"), names(wells))
  if (length(masked) == 1L) control <- control & !(as.logical(wells[[masked]]) %in% TRUE)
  control
}


# Treated wells in a group (cell line and Duration) that has no control well on any
# plate. The processing normalises a treated well with the controls of its group, so
# without them its normalised values are all NA, or it stops with an error that points
# elsewhere. Controls on another plate of the same group are enough.
#' @keywords internal
.check_control_wells <- function(wells, headers) {
  drug <- gDRutils::get_env_identifiers("drug")
  if (!drug %in% names(wells)) return(NULL)
  control <- .is_control_well(wells)
  masked <- intersect(gDRutils::get_env_identifiers("masked_tag"), names(wells))
  is_masked <- if (length(masked) == 1L) as.logical(wells[[masked]]) %in% TRUE else FALSE
  treated <- !control & !is_masked & !is.na(wells[[drug]]) & nzchar(wells[[drug]])
  if (!any(treated)) return(NULL)

  group_cols <- intersect(c(headers[["cellline"]], headers[["duration"]]), names(wells))
  group <- if (length(group_cols) > 0L) {
    do.call(paste, c(lapply(group_cols, function(col) as.character(wells[[col]])), sep = " at Duration "))
  } else {
    rep("all plates", NROW(wells))
  }
  n_control <- tapply(control, group, sum)
  n_treated <- tapply(treated, group, sum)
  bad <- names(n_treated)[n_treated > 0 & n_control == 0]
  if (length(bad) == 0L) {
    return(.finding("control_wells", "PASS", "Every cell line and Duration with treated wells has control wells."))
  }
  barcode <- as.character(wells[[headers[["barcode"]]]])
  hits <- vapply(bad, function(g) {
    sprintf("%s (%s)", g, .head_list(unique(barcode[group == g & treated]), 3L))
  }, character(1))
  .coded_finding(48L, "control_wells", "FAIL", "C", .head_list(hits, 12L))
}


# Drugs whose every well is at concentration 0. They are read as a treatment without
# a dose series, and the processing stops on them with an error that points
# elsewhere. They are usually controls written as a drug id instead of the untreated
# tag; a real drug with a few wells at 0 is not flagged.
#' @keywords internal
.check_zero_dose_drugs <- function(wells, headers) {
  drug <- gDRutils::get_env_identifiers("drug")
  conc <- gDRutils::get_env_identifiers("concentration")
  if (!all(c(drug, conc) %in% names(wells))) return(NULL)
  untreated <- tolower(gDRutils::get_env_identifiers("untreated_tag"))
  ids <- as.character(wells[[drug]])
  dose <- suppressWarnings(as.numeric(wells[[conc]]))
  keep <- !is.na(ids) & nzchar(ids) & !tolower(ids) %in% untreated & !is.na(dose)
  if (!any(keep)) return(NULL)
  max_dose <- tapply(dose[keep], ids[keep], max)
  bad <- names(max_dose)[max_dose == 0]
  if (length(bad) == 0L) return(NULL)
  template <- headers[["template"]]
  hits <- vapply(bad, function(id) {
    in_files <- if (template %in% names(wells)) {
      sprintf(" in %s", .head_list(unique(as.character(wells[[template]][keep & ids == id])), 3L))
    } else {
      ""
    }
    sprintf("%s (%d well(s)%s)", id, sum(keep & ids == id), in_files)
  }, character(1))
  .coded_finding(49L, "zero_dose_drug", "FAIL", "C", .head_list(hits, 12L))
}


# One row per plate: well count, readout median and sd, the median of its control
# wells (when it has at least 3), its Duration and cell line.
#' @keywords internal
.plate_table <- function(wells, headers) {
  readout <- suppressWarnings(as.numeric(wells$ReadoutValue))
  control <- .is_control_well(wells)
  barcode <- as.character(wells[[headers[["barcode"]]]])
  plates <- unique(barcode)
  rows <- split(seq_len(NROW(wells)), factor(barcode, levels = plates))
  first_value <- function(col) {
    if (is.null(col) || !col %in% names(wells)) return(rep(NA_character_, length(plates)))
    vapply(rows, function(i) as.character(wells[[col]][i[1]]), character(1))
  }
  data.table::data.table(
    barcode = plates,
    n_wells = lengths(rows),
    median = vapply(rows, function(i) stats::median(readout[i], na.rm = TRUE), numeric(1)),
    sd = vapply(rows, function(i) stats::sd(readout[i], na.rm = TRUE), numeric(1)),
    control_median = vapply(rows, function(i) {
      k <- i[control[i] & !is.na(readout[i])]
      if (length(k) >= 3L) stats::median(readout[k]) else NA_real_
    }, numeric(1)),
    duration = first_value(headers[["duration"]]),
    cellline = first_value(headers[["cellline"]])
  )
}


#' @keywords internal
.check_readouts <- function(wells, plates) {
  readout <- suppressWarnings(as.numeric(wells$ReadoutValue))
  out <- list()

  drug <- gDRutils::get_env_identifiers("drug")
  annotated <- if (drug %in% names(wells)) !is.na(wells[[drug]]) & nzchar(wells[[drug]]) else TRUE
  n_na <- sum(is.na(readout) & annotated)
  if (n_na > 0L) {
    out <- c(out, list(.finding("readout_values", "WARN", sprintf(
      "%d treated or control well(s) have no readout value.", n_na
    ), "C")))
  }
  n_negative <- sum(readout < 0, na.rm = TRUE)
  if (n_negative > 0L) out <- c(out, list(.coded_finding(47L, "negative_readouts", "FAIL", "C", n_negative)))
  n_zero <- sum(readout == 0, na.rm = TRUE)
  if (n_zero > 0L) {
    share <- n_zero / length(readout)
    out <- c(out, list(.finding("zero_readouts", if (share > 0.1) "WARN" else "INFO", sprintf(
      "%d readout value(s) are exactly 0 (%.1f%%).", n_zero, 100 * share
    ), if (share > 0.1) "C" else NA_character_)))
  }
  if ("BackgroundValue" %in% names(wells)) {
    background <- suppressWarnings(as.numeric(wells$BackgroundValue))
    n_bg <- sum(background >= readout & readout > 0, na.rm = TRUE)
    if (n_bg > 0L) {
      out <- c(out, list(.finding("background", "WARN", sprintf(
        "%d well(s) have a background value at or above the readout.", n_bg
      ), "C")))
    }
  }
  usual <- as.integer(names(which.max(table(plates$n_wells))))
  odd <- plates[plates$n_wells != usual, ]
  if (NROW(odd) > 0L) {
    out <- c(out, list(.finding("well_count", "WARN", sprintf(
      "%d plate(s) have a different number of wells than the usual %d: %s",
      NROW(odd), usual, .head_list(sprintf("%s (%d)", odd$barcode, odd$n_wells))
    ), "C")))
  }
  flat <- plates$barcode[is.finite(plates$sd / abs(plates$median)) & plates$sd / abs(plates$median) < 0.01]
  if (length(flat) > 0L) {
    out <- c(out, list(.finding("flat_plates", "WARN", sprintf(
      "Plate(s) with almost constant readouts, which usually means a failed read: %s", .head_list(flat)
    ), "C")))
  }
  .bind_findings(out)
}


# Plates whose median readout is more than 3.5 MADs from comparable plates: same
# Duration and cell line, in groups of at least 4 plates. Plates without a Duration or
# a cell line in the manifest are left out, as in .check_duration_plausibility():
# grouping them together would compare different cell lines.
#' @keywords internal
.check_plate_outliers <- function(plates) {
  known <- !is.na(plates$duration) & nzchar(plates$duration) & !is.na(plates$cellline) & nzchar(plates$cellline)
  group <- paste(plates$duration, plates$cellline, sep = "|")
  scored <- FALSE
  outliers <- character(0)
  for (g in split(which(known), group[known])) {
    if (length(g) < 4L) next
    med <- plates$median[g]
    spread <- stats::mad(med, na.rm = TRUE)
    if (!is.finite(spread) || spread == 0) next
    scored <- TRUE
    score <- abs(med - stats::median(med, na.rm = TRUE)) / spread
    outliers <- c(outliers, plates$barcode[g][is.finite(score) & score > 3.5])
  }
  if (length(outliers) > 0L) {
    return(.finding("plate_outliers", "WARN", sprintf(
      "Plate(s) read far from plates with the same Duration and cell line (more than 3.5 MAD): %s",
      .head_list(outliers)
    ), "C"))
  }
  if (scored) return(.finding("plate_outliers", "PASS", "No plate reads far from comparable plates."))
  .finding("plate_outliers", "INFO", "Not checked: no group of 4 or more plates with the same Duration and cell line.")
}


# Duration drives GR normalization, and a mislabelled plate (a day-0 plate labelled
# 168) passes every other check while making GR values wrong. Two signals:
#   1. a cell line whose set of Durations differs from the set most cell lines have;
#   2. within a cell line, control wells reading more than 20% lower at a longer
#      Duration, when the cells had more time to grow. Only control wells are used: a
#      potent drug can bring a treated plate below day 0 with correct labels. A smaller
#      drop is normal once the cells reach confluence.
#' @keywords internal
.check_duration_plausibility <- function(plates) {
  duration <- suppressWarnings(as.numeric(plates$duration))
  cellline <- plates$cellline
  keep <- !is.na(duration) & !is.na(cellline) & nzchar(cellline)
  duration <- duration[keep]
  cellline <- cellline[keep]
  control <- plates$control_median[keep]
  if (length(unique(duration)) < 2L) return(NULL)
  out <- list()

  sets <- vapply(split(duration, cellline), function(d) paste(sort(unique(d)), collapse = ","), character(1))
  usual <- names(which.max(table(sets)))
  odd <- names(sets)[sets != usual]
  if (length(sets) >= 2L && length(odd) > 0L && length(odd) < length(sets)) {
    out <- c(out, list(.finding("duration_plausibility", "WARN", sprintf(
      paste0("Cell line(s) do not have the Durations most cell lines have (%s), ",
             "which is what a mislabelled plate leaves behind: %s"),
      usual, .head_list(sprintf("%s has %s", odd, sets[odd]))
    ), "C")))
  }

  falling <- character(0)
  for (cl in unique(cellline)) {
    i <- cellline == cl & !is.na(control)
    by_duration <- tapply(control[i], duration[i], stats::median)
    if (length(by_duration) < 2L) next
    by_duration <- by_duration[order(as.numeric(names(by_duration)))]
    if (any(by_duration[-1] < 0.8 * by_duration[-length(by_duration)])) {
      falling <- c(falling, sprintf("%s (%s)", cl, paste(
        sprintf("%sh: %.0f", names(by_duration), by_duration), collapse = " -> "
      )))
    }
  }
  if (length(falling) > 0L) {
    out <- c(out, list(.finding("duration_plausibility", "WARN", sprintf(
      "Control wells read more than 20%% lower at a longer Duration, so a plate may have the wrong Duration: %s",
      .head_list(falling, 5L)
    ), "C")))
  }
  .bind_findings(out)
}


####
# PRISM, MAE and D300 input
####

# The PRISM role of a table from its columns, or NA.
#' @keywords internal
.prism_role <- function(cols) {
  synonyms <- .prism_level5_column_synonyms()
  has_lfc <- any(synonyms$LFC_cb %in% cols)
  if ("ModelID" %in% cols && any(c("CCLEName", "OncotreeLineage") %in% cols)) return("prism_meta")
  if (has_lfc && any(c("ccle_name", "depmap_id") %in% cols) && any(synonyms$pert_iname %in% cols)) {
    return("prism_data")
  }
  if (length(cols) > 1L && cols[1] == "V1" && !all(grepl("^V[0-9]+$", cols))) return("prism_data")
  if (has_lfc && any(c("row_id", "row_name") %in% cols) && any(c("profile_id", "column_name") %in% cols)) {
    return("prism_data")
  }
  if (!has_lfc && any(c("row_name", "row_id", "depmap_id") %in% cols)) return("prism_cell_lines")
  if (!has_lfc && any(c("column_name", "profile_id", "SampleID") %in% cols)) return("prism_treatment")
  NA_character_
}


#' @keywords internal
.validate_prism <- function(files) {
  path_of <- function(role) files$path[files$role == role]
  meta <- path_of("prism_meta")
  data_files <- path_of("prism_data")
  out <- list(.safely(.check_encoding(files)))

  required_meta <- c("ModelID", "CCLEName", "OncotreeLineage")
  if (length(meta) == 0L) {
    out <- c(out, list(.finding("prism_meta", "FAIL", sprintf(
      "No PRISM model metadata file (%s) was found.", toString(required_meta)
    ), "C")))
  } else {
    missing <- setdiff(required_meta, .peek_columns(meta[1]))
    out <- c(out, list(if (length(missing) > 0L) {
      .finding("prism_meta", "FAIL", sprintf("%s misses column(s) %s.", basename(meta[1]), toString(missing)), "C")
    } else {
      .finding("prism_meta", "PASS", sprintf("%s has %s.", basename(meta[1]), toString(required_meta)))
    }))
  }

  if (length(data_files) == 0L) {
    return(.bind_findings(out, .finding("prism_data", "FAIL", "No PRISM LFC data file was found.", "C")))
  }
  for (path in data_files) {
    out <- c(out, list(.check_prism_data(path, has_cell_lines = any(files$role == "prism_cell_lines"),
                                         has_treatment = any(files$role == "prism_treatment"))))
  }
  .bind_findings(out)
}


# Check a PRISM data file against what the LEVEL5 or LEVEL6 converter requires, after
# the converter's own column synonyms.
#' @keywords internal
.check_prism_data <- function(path, has_cell_lines, has_treatment) {
  cols <- .peek_columns(path)
  synonyms <- .prism_level5_column_synonyms()
  for (target in names(synonyms)) {
    present <- intersect(synonyms[[target]], cols)
    if (!target %in% cols && length(present) > 0L) cols[cols == present[1]] <- target
  }
  file <- basename(path)
  if (any(c("ccle_name", "depmap_id") %in% cols) && "pert_iname" %in% cols) {
    missing <- setdiff(c("pert_iname", "pert_dose", "pert_time", "LFC_cb"), cols)
    return(.bind_findings(
      .finding("prism_level", "PASS", sprintf("%s is PRISM LEVEL5 data.", file)),
      if (length(missing) > 0L) {
        .finding("prism_data", "FAIL", sprintf("%s misses column(s) %s.", file, toString(missing)), "C")
      }
    ))
  }
  if ((length(cols) > 1L && cols[1] == "V1") ||
        ("LFC_cb" %in% cols && all(c("row_id", "profile_id") %in% cols))) {
    return(.bind_findings(
      .finding("prism_level", "PASS", sprintf("%s is PRISM LEVEL6 data.", file)),
      if (!has_cell_lines) {
        .finding("prism_data", "FAIL", "LEVEL6 data needs a cell line file (row_name or depmap_id).", "C")
      },
      if (!has_treatment) {
        .finding("prism_data", "FAIL", "LEVEL6 data needs a treatment file (column_name or profile_id).", "C")
      }
    ))
  }
  .finding("prism_level", "WARN", sprintf(
    "Could not tell the PRISM level of %s from its columns: %s", file, .head_list(cols, 10L)
  ), "C")
}


#' @keywords internal
.validate_mae <- function(files) {
  mae <- files$path[files$role == "mae"]
  if (length(mae) == 0L) return(.finding("mae", "FAIL", "No .qs2 file was found.", "C"))
  obj <- .quietly(qs2::qs_read(mae[1]))
  if (!is.null(obj$error)) {
    return(.finding("mae", "FAIL", sprintf("%s could not be read: %s", basename(mae[1]), obj$error), "C"))
  }
  check <- .quietly(gDRutils::validate_MAE(obj$value))
  .bind_findings(
    if (!is.null(check$error)) {
      .finding("mae", "FAIL", sprintf("%s is not a valid gDR MAE: %s", basename(mae[1]), check$error), "C")
    } else {
      .finding("mae", "PASS", sprintf("%s is a valid gDR MAE.", basename(mae[1])))
    },
    if (length(mae) > 1L) {
      .finding("mae", "INFO", sprintf("Only %s was checked of %d .qs2 files.", basename(mae[1]), length(mae)))
    }
  )
}


#' @keywords internal
.validate_pre_d300 <- function(files) {
  .finding("stage", "INFO", sprintf(
    "%d D300 file(s) (.tdd) but no treatment templates: generate the templates with import_D300() first.",
    sum(files$role == "d300")
  ), "B")
}
