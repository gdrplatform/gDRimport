make_long_table <- function(...) {
  dt <- data.table::data.table(
    Gnumber = c("G1", "G1", "G2"),
    clid = c("CL1", "CL1", "CL2"),
    Duration = c(72, 72, 72),
    Concentration = c(0, 1, 1),
    ReadoutValue = c(1000, 500, 400)
  )
  extra <- list(...)
  for (nm in names(extra)) {
    dt[[nm]] <- extra[[nm]]
  }
  dt
}

test_that("load_long_table reads a valid CSV", {
  path <- tempfile(fileext = ".csv")
  data.table::fwrite(make_long_table(), path)

  obs <- gDRimport::load_long_table(path)
  expect_s3_class(obs, "data.table")
  expect_equal(NROW(obs), 3)
  expect_true(all(c("Gnumber", "clid", "Duration", "Concentration", "ReadoutValue")
                  %in% colnames(obs)))
  expect_type(obs$ReadoutValue, "double")
  expect_type(obs$Concentration, "double")
})

test_that("load_long_table reads a TSV (separator autodetected)", {
  path <- tempfile(fileext = ".tsv")
  data.table::fwrite(make_long_table(), path, sep = "\t")

  obs <- gDRimport::load_long_table(path)
  expect_equal(NROW(obs), 3)
})

test_that("load_long_table errors on missing required columns", {
  path <- tempfile(fileext = ".csv")
  dt <- make_long_table()
  dt$Concentration <- NULL
  data.table::fwrite(dt, path)

  expect_error(gDRimport::load_long_table(path), "Concentration")
})

test_that("load_long_table errors on non-numeric readout", {
  path <- tempfile(fileext = ".csv")
  dt <- make_long_table()
  dt$ReadoutValue <- c("1000", "high", "400")
  data.table::fwrite(dt, path)

  expect_error(gDRimport::load_long_table(path), "ReadoutValue")
})

test_that("load_long_table errors on non-numeric duration", {
  path <- tempfile(fileext = ".csv")
  dt <- make_long_table()
  dt$Duration <- c("72", "72", "later")
  data.table::fwrite(dt, path)

  expect_error(gDRimport::load_long_table(path), "Duration")
})

test_that("load_long_table errors on non-numeric concentration", {
  path <- tempfile(fileext = ".csv")
  dt <- make_long_table()
  dt$Concentration <- c("0", "1", "lots")
  data.table::fwrite(dt, path)

  expect_error(gDRimport::load_long_table(path), "Concentration")
})

test_that("load_long_table errors on non-numeric combination concentration", {
  path <- tempfile(fileext = ".csv")
  dt <- make_long_table(Gnumber_2 = "G3", Concentration_2 = c("0", "0.5", "some"))
  data.table::fwrite(dt, path)

  expect_error(gDRimport::load_long_table(path), "Concentration_2")
})

test_that("load_long_table surfaces a parse error for an unparsable file", {
  # a directory is readable but data.table::fread cannot parse it, so the
  # tryCatch around fread rethrows as the long-table parse exception
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  expect_error(gDRimport::load_long_table(path), "long table could not be parsed")
})

test_that("load_long_table errors on empty table", {
  path <- tempfile(fileext = ".csv")
  data.table::fwrite(make_long_table()[0], path)

  expect_error(gDRimport::load_long_table(path), "empty")
})

test_that("load_long_table validates input arguments", {
  expect_error(gDRimport::load_long_table(123), "Must be of type 'character'")

  path <- tempfile(fileext = ".csv")
  data.table::fwrite(make_long_table(), path)
  expect_error(gDRimport::load_long_table(c(path, path)), "length 1")
})

test_that("load_long_table accepts optional combination columns", {
  path <- tempfile(fileext = ".csv")
  data.table::fwrite(
    make_long_table(Gnumber_2 = "G3", Concentration_2 = c(0, 0.5, 0.5)),
    path
  )

  obs <- gDRimport::load_long_table(path)
  expect_true(all(c("Gnumber_2", "Concentration_2") %in% colnames(obs)))
  expect_type(obs$Concentration_2, "double")
})
