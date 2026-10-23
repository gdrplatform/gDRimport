# Helpers for test-validate_input.R. Each test works on copies of the package test
# data in a temporary directory, so it can corrupt one file without touching the
# installed package.

# Copy `paths` into a new temporary directory; returns the paths of the copies, named
# by file name.
copy_inputs <- function(paths) {
  dir <- tempfile("gdr_validate_")
  dir.create(dir)
  file.copy(paths, dir)
  stats::setNames(file.path(dir, basename(paths)), basename(paths))
}

data1_inputs <- function() {
  td <- get_test_data()
  c(manifest_path(td), template_path(td), result_path(td))
}

data2_inputs <- function() {
  td <- get_test_Tecan_data()
  c(td$m_file, td$t_files, td$r_files)
}

data4_inputs <- function() {
  td <- get_test_EnVision_data()
  c(td$m_file, td$t_files, td$r_files)
}

data5_inputs <- function() {
  td <- get_test_tsv_data()
  c(td$m_file, td$t_files, td$r_files)
}

# Write `value` into one cell of an xlsx sheet (NA clears the cell).
set_xlsx_cell <- function(path, sheet, row, col, value) {
  wb <- openxlsx::loadWorkbook(path)
  if (is.na(value)) {
    openxlsx::deleteData(wb, sheet, cols = col, rows = row, gridExpand = TRUE)
  } else {
    openxlsx::writeData(wb, sheet, value, startCol = col, startRow = row, colNames = FALSE)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

# Apply `fn` to a tab-separated table and write it back.
edit_tsv <- function(path, fn) {
  dt <- data.table::fread(path, sep = "\t", colClasses = "character")
  data.table::fwrite(fn(dt), path, sep = "\t")
}

# The findings with this check (and severity); fails when there is none.
expect_finding <- function(res, check, severity, status_code = NULL) {
  # computed outside `[` so that data.table does not read `check` as the column
  keep <- res$findings$check == check & res$findings$severity == severity
  hit <- res$findings[keep]
  expect_gt(NROW(hit), 0L, label = sprintf("number of %s findings for '%s'", severity, check))
  if (!is.null(status_code)) expect_true(status_code %in% hit$status_code)
  invisible(hit)
}

expect_no_finding <- function(res, check) {
  expect_false(check %in% res$findings$check, label = sprintf("a finding for '%s'", check))
}

# No WARN or FAIL besides the checks in `allowed`; a failure names the findings.
expect_no_warn_fail <- function(res, allowed = character(0)) {
  f <- res$findings
  keep <- f$severity %in% c("WARN", "FAIL") & !f$check %in% allowed
  bad <- f[keep]
  expect_identical(sprintf("%s %s: %s", bad$severity, bad$check, bad$detail), character(0))
}
