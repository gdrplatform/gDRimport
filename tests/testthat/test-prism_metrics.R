prism_se <- function() {
  mae <- gDRutils::get_synthetic_data("finalMAE_prism")
  MultiAssayExperiment::experiments(mae)[["single-agent"]]
}
drc_path <- system.file("testdata/prism_drc.csv", package = "gDRimport")
model_path <- system.file("testdata/prism_drc_model.csv", package = "gDRimport")
prism_rows <- function(se) {
  m <- gDRutils::convert_se_assay_to_dt(se, "Metrics")
  m[m$fit_source == "PRISM", ]
}

test_that("add_PRISM_metrics imports every PRISM curve with the gDR curve parameters", {
  se <- prism_se()
  gdr_before <- gDRutils::convert_se_assay_to_dt(se, "Metrics")
  out <- add_PRISM_metrics(se, drc_path, model_path)
  drc <- data.table::fread(drc_path)

  prism <- prism_rows(out)
  expect_identical(NROW(prism), NROW(drc))
  data.table::setorder(prism, lua)
  expect_identical(prism$lua, drc$lua)
  expect_identical(unique(prism$normalization_type), "RV")
  expect_equal(prism$x_0, drc$upper_limit)
  expect_equal(prism$x_inf, drc$lower_limit)
  expect_equal(prism$ec50, drc$inflection)
  expect_equal(prism$h, -drc$slope)
  expect_equal(prism$r2, drc$frac_var_explained)
  expect_equal(prism$maxlog10Concentration, log10(drc$maximum_dose))
  expect_equal(prism$prism_auc, drc$auc)
  expect_equal(prism$prism_ic50, drc$ic50)
  expect_identical(prism$fit_type, c(drc$fit_name[1:3], NA))
  expect_true(all(is.na(prism$xc50)) && all(is.na(prism$x_AOC)))
  # two curves of one cell line, told apart by pool and compound plate
  expect_identical(sum(prism$CellLineName == "HEL9217_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE"), 2L)
  expect_setequal(prism$pool_id, c("P1", "P2"))
  expect_true(all(prism$N_conc > 0))

  # the gDR fits stay as they were
  gdr_after <- gDRutils::convert_se_assay_to_dt(out, "Metrics")
  gdr_after <- gdr_after[gdr_after$fit_source == "gDR", names(gdr_before), with = FALSE]
  data.table::setorderv(gdr_after, c("rId", "cId", "normalization_type"))
  data.table::setorderv(gdr_before, c("rId", "cId", "normalization_type"))
  expect_equal(gdr_after, gdr_before)
})

test_that("the imported parameters give PRISM's curve", {
  out <- add_PRISM_metrics(prism_se(), drc_path, model_path)
  p <- prism_rows(out)[lua == "LUA-0001"]
  drc <- data.table::fread(drc_path)[lua == "LUA-0001"]
  dose <- 10 ^ seq(-3, 1, 0.5)
  prism_curve <- drc$upper_limit + (drc$lower_limit - drc$upper_limit) / (1 + (dose / drc$inflection) ^ drc$slope)
  gdr_curve <- gDRutils::predict_efficacy_from_conc(dose, p$x_inf, p$x_0, p$ec50, p$h)
  expect_equal(gdr_curve, prism_curve)
})

test_that("add_PRISM_metrics replaces the PRISM rows when run again and records the import", {
  out <- add_PRISM_metrics(prism_se(), drc_path, model_path)
  n <- NROW(gDRutils::convert_se_assay_to_dt(out, "Metrics"))
  again <- add_PRISM_metrics(out, drc_path, model_path)
  expect_identical(NROW(gDRutils::convert_se_assay_to_dt(again, "Metrics")), n)

  prov <- S4Vectors::metadata(again)[["provenance"]]
  expect_true("PRISM" %in% names(prov$fits[["Metrics"]]))
  step <- prov$steps[["prism_drc_import"]]$settings
  expect_identical(step$n_curves, 4L)
  expect_identical(step$n_imported, 4L)
  expect_identical(step$md5, unname(tools::md5sum(drc_path)))
  expect_identical(step$screen, "TEST_SCREEN")
})

test_that("add_PRISM_metrics leaves out curves without a cell and maps drug identifiers", {
  dir <- tempfile()
  dir.create(dir)
  drc <- data.table::fread(drc_path)
  drc$pert_name <- "BRD-0001"
  extra <- drc[1]
  extra$depmap_id <- "ACH-999999"
  path <- file.path(dir, "drc.csv")
  data.table::fwrite(rbind(drc, extra), path)

  expect_warning(out <- add_PRISM_metrics(prism_se(), path, model_path, drug_map = c(`BRD-0001` = "G00106")),
                 "1 of 5 PRISM curves have no matching cell")
  expect_identical(NROW(prism_rows(out)), 4L)
  expect_identical(S4Vectors::metadata(out)$provenance$steps$prism_drc_import$settings$n_left_out, 1L)

  # without the map, no drug matches
  expect_error(purrr::quietly(add_PRISM_metrics)(prism_se(), path, model_path), "no PRISM curve matches")
})

test_that("add_PRISM_metrics checks its input", {
  se <- prism_se()
  expect_error(add_PRISM_metrics(se, "no_such_file.csv", model_path), "File does not exist")
  expect_error(add_PRISM_metrics(se, drc_path), "meta_data_path")
  dir <- tempfile()
  dir.create(dir)
  path <- file.path(dir, "drc.csv")
  data.table::fwrite(data.table::fread(drc_path)[, -"inflection"], path)
  expect_error(add_PRISM_metrics(se, path, model_path), "inflection")
})
