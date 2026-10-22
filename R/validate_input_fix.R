####
# Repairs for validate_gdr_input(fix = TRUE)
####
#
# With fix = TRUE the input files are copied to an output directory, the mechanical
# (tier A) problems are repaired in the copies, and the copies are validated again.
# The user's files are never written. Each repair works on a table of files (the one
# .classify_input_files() returns) and returns the updated table with a note per change.
# They run in a fixed order, because later repairs need the results of earlier ones:
# the manifest references are matched last, against the final template names.


# Copy the inputs to `output_dir`, repair the copies, and validate them again.
#' @keywords internal
.validate_and_fix <- function(files, type, instrument, id_patterns, output_dir) {
  .prepare_output_dir(output_dir)
  if (anyDuplicated(files$file) > 0L) {
    stop(sprintf("Input files share a name, so they cannot be copied to one directory: %s",
                 toString(unique(files$file[duplicated(files$file)]))), call. = FALSE)
  }
  copies <- data.table::copy(files)
  copies$path <- file.path(normalizePath(output_dir), files$file)
  file.copy(files$path, copies$path)

  notes <- character(0)
  fixers <- list(.fix_encoding, .fix_file_names, .fix_long_tables, .fix_sheet_names,
                 .fix_border_placeholders, .fix_template_references)
  for (fixer in fixers) {
    step <- fixer(copies)
    copies <- step$files
    notes <- c(notes, step$notes)
  }

  res <- validate_gdr_input(output_dir, type = type, instrument = instrument, id_patterns = id_patterns)
  res$fixes <- notes
  res$output_dir <- normalizePath(output_dir)
  res
}


# `output_dir` must be new or empty, so a repair can never overwrite a file.
#' @keywords internal
.prepare_output_dir <- function(output_dir) {
  if (file.exists(output_dir) && !dir.exists(output_dir)) {
    stop(sprintf("'output_dir' is a file: %s", output_dir), call. = FALSE)
  }
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE)) > 0L) {
    stop(sprintf("'output_dir' must be a new or empty directory: %s", output_dir), call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  invisible(output_dir)
}


#' @keywords internal
.fixed <- function(files, notes = character(0)) {
  list(files = files, notes = notes)
}


# Text files that are not valid UTF-8 are read as Windows-1252, the usual encoding of
# plate-reader and Excel exports on Windows (Latin-1 when that fails), and written as
# UTF-8.
#' @keywords internal
.fix_encoding <- function(files) {
  notes <- character(0)
  text <- which(.file_ext(files$path) %in% c("csv", "tsv", "txt"))
  for (i in text[!vapply(files$path[text], .is_valid_utf8, logical(1))]) {
    bytes <- readBin(files$path[i], "raw", file.size(files$path[i]))
    content <- tryCatch(rawToChar(bytes), error = function(e) NA_character_)
    if (is.na(content)) next
    encoding <- "Windows-1252"
    utf8 <- iconv(content, from = "WINDOWS-1252", to = "UTF-8")
    if (is.na(utf8)) {
      encoding <- "Latin-1"
      utf8 <- iconv(content, from = "latin1", to = "UTF-8")
    }
    if (is.na(utf8)) next
    writeBin(charToRaw(utf8), files$path[i])
    notes <- c(notes, sprintf("%s: re-encoded from %s to UTF-8", files$file[i], encoding))
  }
  .fixed(files, notes)
}


# Spaces at the start or end of a file name, or before its extension, are removed.
#' @keywords internal
.fix_file_names <- function(files) {
  notes <- character(0)
  for (i in which(grepl("^\\s|\\s$|\\s\\.[A-Za-z0-9]+$", files$file))) {
    name <- trimws(files$file[i])
    ext <- tools::file_ext(name)
    new_name <- if (nzchar(ext)) paste0(trimws(tools::file_path_sans_ext(name)), ".", ext) else name
    new_path <- file.path(dirname(files$path[i]), new_name)
    if (file.exists(new_path)) next
    file.rename(files$path[i], new_path)
    notes <- c(notes, sprintf("'%s' renamed to '%s'", files$file[i], new_name))
    files$file[i] <- new_name
    files$path[i] <- new_path
  }
  .fixed(files, notes)
}


# Long tables (one row per well) in xlsx or csv are saved as tab-separated .tsv files,
# the only format the loader reads them from.
#' @keywords internal
.fix_long_tables <- function(files) {
  notes <- character(0)
  for (i in which(files$role %in% c("template_long", "raw_long"))) {
    path <- files$path[i]
    new_path <- sub("\\.[^.]+$", ".tsv", path)
    if (file.exists(new_path)) next
    # read as text, so the copy keeps every value as written (a column that turns from
    # numbers to text after the first rows, leading zeros) and the loader parses it
    table <- if (.file_ext(path) %in% c("xlsx", "xls")) {
      data.table::as.data.table(readxl::read_excel(path, col_types = "text"))
    } else {
      data.table::fread(path, colClasses = "character")
    }
    data.table::fwrite(table, new_path, sep = "\t")
    file.remove(path)
    notes <- c(notes, sprintf("%s: long table saved as %s", files$file[i], basename(new_path)))
    files$file[i] <- basename(new_path)
    files$path[i] <- new_path
    files$role[i] <- if (files$role[i] == "template_long") "template" else "raw"
    files$format[i] <- if (files$role[i] == "raw") "long_tsv" else NA_character_
  }
  .fixed(files, notes)
}


# Template sheets named Gnumber_1 / Concentration_1 are renamed to Gnumber /
# Concentration, unless the file already has a sheet with that name.
#' @keywords internal
.fix_sheet_names <- function(files) {
  notes <- character(0)
  targets <- unlist(gDRutils::get_env_identifiers(c("drug", "concentration"), simplify = FALSE),
                    use.names = FALSE)
  for (i in which(files$role == "template" & .file_ext(files$path) == "xlsx")) {
    sheets <- readxl::excel_sheets(files$path[i])
    renames <- list()
    for (target in targets) {
      old <- sheets[tolower(sheets) == tolower(paste0(target, "_1"))]
      if (length(old) == 1L && !target %in% sheets) renames[[old]] <- target
    }
    if (length(renames) == 0L) next
    wb <- openxlsx::loadWorkbook(files$path[i])
    for (old in names(renames)) openxlsx::renameWorksheet(wb, old, renames[[old]])
    openxlsx::saveWorkbook(wb, files$path[i], overwrite = TRUE)
    notes <- c(notes, sprintf("%s: sheet %s renamed to %s", files$file[i], names(renames), unlist(renames)))
  }
  .fixed(files, notes)
}


# 'n/a' and '-99' in templates are cleared. In xlsx only those cells are deleted, at
# the coordinates the loader reads them from, so no other cell moves or changes type.
#' @keywords internal
.fix_border_placeholders <- function(files) {
  notes <- character(0)
  for (i in which(files$role == "template")) {
    path <- files$path[i]
    template <- .read_templates(path)[[1]]
    n_cleared <- 0L
    if (.file_ext(path) == "xlsx") {
      wb <- NULL
      for (s in seq_along(template$sheets)) {
        cells <- template$sheets[[s]]
        if (is.null(cells)) next
        # .is_placeholder_cell() drops the dimensions, so the matrix is rebuilt
        hit <- which(matrix(.is_placeholder_cell(cells), nrow = NROW(cells)), arr.ind = TRUE)
        if (NROW(hit) == 0L) next
        if (is.null(wb)) wb <- openxlsx::loadWorkbook(path)
        openxlsx::deleteData(wb, sheet = s, cols = hit[, "col"], rows = hit[, "row"], gridExpand = FALSE)
        n_cleared <- n_cleared + NROW(hit)
      }
      if (!is.null(wb)) openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    } else if (.file_ext(path) == "tsv") {
      table <- data.table::fread(path, sep = "\t", colClasses = "character")
      for (col in names(table)) {
        hit <- .is_placeholder_cell(table[[col]])
        if (!any(hit)) next
        data.table::set(table, which(hit), col, "")
        n_cleared <- n_cleared + sum(hit)
      }
      if (n_cleared > 0L) data.table::fwrite(table, path, sep = "\t")
    }
    if (n_cleared > 0L) {
      notes <- c(notes, sprintf("%s: %d 'n/a' or '-99' cell(s) cleared", files$file[i], n_cleared))
    }
  }
  .fixed(files, notes)
}


# Manifest references to templates that only differ from a template file by spaces or
# the extension are rewritten to the file name. In xlsx only the changed cells are
# written.
#' @keywords internal
.fix_template_references <- function(files) {
  notes <- character(0)
  present <- files$file[files$role == "template"]
  bare <- function(x) trimws(tools::file_path_sans_ext(trimws(x)))
  template_ids <- gDRutils::get_env_identifiers("template")
  for (i in which(files$role == "manifest" & .file_ext(files$path) %in% c("xlsx", "tsv"))) {
    path <- files$path[i]
    is_xlsx <- .file_ext(path) == "xlsx"
    cells <- if (is_xlsx) {
      as.matrix(readxl::read_excel(path, range = readxl::cell_limits(c(1L, 1L), c(NA, NA)),
                                   col_names = FALSE, col_types = "text", .name_repair = "minimal"))
    } else {
      as.matrix(data.table::fread(path, sep = "\t", header = FALSE, colClasses = "character"))
    }
    # the header row is the first one with a template column
    header <- which(apply(cells, 1, function(r) {
      any(gDRutils::update_idfs_synonyms(trimws(r)) %in% template_ids, na.rm = TRUE)
    }))[1]
    if (is.na(header)) next
    col <- which(gDRutils::update_idfs_synonyms(trimws(cells[header, ])) %in% template_ids)[1]
    rows <- seq_len(NROW(cells))[-seq_len(header)]
    changed <- character(0)
    wb <- NULL
    for (r in rows) {
      ref <- cells[r, col]
      if (is.na(ref) || ref %in% present) next
      match <- present[bare(present) == bare(ref)]
      if (length(match) != 1L) next
      cells[r, col] <- match
      changed <- c(changed, sprintf("%s: template reference '%s' changed to '%s'", files$file[i], ref, match))
      if (is_xlsx) {
        if (is.null(wb)) wb <- openxlsx::loadWorkbook(path)
        openxlsx::writeData(wb, sheet = 1, x = match, startCol = col, startRow = r, colNames = FALSE)
      }
    }
    if (length(changed) == 0L) next
    if (is_xlsx) {
      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    } else {
      data.table::fwrite(data.table::as.data.table(cells), path, sep = "\t", col.names = FALSE)
    }
    # one note per reference, however many manifest rows use it
    notes <- c(notes, unique(changed))
  }
  .fixed(files, notes)
}
