drc_path <- system.file("testdata/prism_drc.csv", package = "gDRimport")
model_path <- system.file("testdata/prism_drc_model.csv", package = "gDRimport")

test_that("read_PRISM_drc returns the curves as delivered, with cell line and Duration", {
  curves <- read_PRISM_drc(drc_path, model_path)
  raw <- data.table::fread(drc_path, colClasses = list(character = c("pert_name", "depmap_id")))
  idfs <- gDRutils::get_env_identifiers()

  expect_identical(NROW(curves), NROW(raw))
  expect_equal(curves[, names(raw), with = FALSE], raw, check.attributes = FALSE)
  model <- data.table::fread(model_path)
  expect_identical(curves[[idfs$cellline]], model$CCLEName[match(raw$depmap_id, model$ModelID)])
  expect_identical(unique(curves[[idfs$duration]]), 120)

  record <- get_import_provenance(curves)
  expect_identical(record$source$format, "PRISM DRC")
  expect_identical(record$files$role, c("prism_drc", "meta_data"))
  expect_identical(record$files$md5[1], unname(tools::md5sum(drc_path)))
})

test_that("read_PRISM_drc leaves the cell line NA without a CCLE name and uses ccle_name when given", {
  dir <- tempfile()
  dir.create(dir)
  model <- data.table::fread(model_path)
  model$CCLEName[1] <- ""
  path <- file.path(dir, "model.csv")
  data.table::fwrite(model, path)
  curves <- read_PRISM_drc(drc_path, path)
  expect_identical(sum(is.na(curves[[gDRutils::get_env_identifiers("cellline")]])), 1L)

  drc <- data.table::fread(drc_path)
  drc$ccle_name <- paste0("line_", seq_len(NROW(drc)))
  drc_ccle <- file.path(dir, "drc.csv")
  data.table::fwrite(drc, drc_ccle)
  expect_identical(read_PRISM_drc(drc_ccle)[[gDRutils::get_env_identifiers("cellline")]], drc$ccle_name)
})

test_that("read_PRISM_drc checks its input", {
  expect_error(read_PRISM_drc("no_such_file.csv", model_path), "File does not exist")
  expect_error(read_PRISM_drc(drc_path), "meta_data_path")
  dir <- tempfile()
  dir.create(dir)
  path <- file.path(dir, "drc.csv")
  data.table::fwrite(data.table::fread(drc_path)[, -"inflection"], path)
  expect_error(read_PRISM_drc(path, model_path), "inflection")
})
