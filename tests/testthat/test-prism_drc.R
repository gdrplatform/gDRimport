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
  # without depmap_id
  data.table::fwrite(drc[, -"depmap_id"], drc_ccle)
  expect_no_warning(curves <- read_PRISM_drc(drc_ccle))
  expect_identical(curves[[gDRutils::get_env_identifiers("cellline")]], drc$ccle_name)
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

l2fc_path <- system.file("testdata/prism_l2fc.csv", package = "gDRimport")

test_that("read_PRISM_l2fc keeps every replicate point uncapped, with cell line and Duration", {
  points <- read_PRISM_l2fc(l2fc_path, model_path)
  raw <- data.table::fread(l2fc_path, colClasses = list(character = c("pert_name", "depmap_id", "pool_id",
                                                                        "lua", "pert_id")))
  idfs <- gDRutils::get_env_identifiers()

  expect_identical(NROW(points), NROW(raw))
  expect_equal(points[, names(raw), with = FALSE], raw, check.attributes = FALSE)
  # no cap: the import caps 2^l2fc at 1.03
  expect_gt(max(2^points$l2fc), 1.03)
  model <- data.table::fread(model_path)
  expect_identical(points[[idfs$cellline]], model$CCLEName[match(raw$depmap_id, model$ModelID)])
  expect_identical(unique(points[[idfs$duration]]), 120)
  expect_identical(data.table::uniqueN(points$lua), 4L)

  record <- get_import_provenance(points)
  expect_identical(record$source$format, "PRISM l2fc")
  expect_identical(record$files$role, c("prism_l2fc", "meta_data"))
  expect_identical(record$files$md5[1], unname(tools::md5sum(l2fc_path)))
})

test_that("read_PRISM_l2fc accepts the LEVEL5 column names and leaves out missing values", {
  dir <- tempfile()
  dir.create(dir)
  raw <- data.table::fread(l2fc_path)
  raw$l2fc[1] <- NA
  data.table::setnames(raw, c("l2fc", "pert_name", "day"), c("LFC", "pert_iname", "pert_time"))
  path <- file.path(dir, "level5.csv")
  data.table::fwrite(raw, path)

  points <- read_PRISM_l2fc(path, model_path)
  expect_true(all(c("l2fc", "pert_name", "day") %in% names(points)))
  expect_identical(NROW(points), NROW(raw) - 1L)

  expect_error(read_PRISM_l2fc(l2fc_path), "meta_data_path")
  data.table::fwrite(raw[, -"pert_dose"], path)
  expect_error(read_PRISM_l2fc(path, model_path), "pert_dose")
})

test_that("the PRISM replicate-level data stay an input of the LEVEL5 import", {
  expect_identical(.prism_role(names(data.table::fread(l2fc_path, nrows = 0L))), "prism_data")
})
