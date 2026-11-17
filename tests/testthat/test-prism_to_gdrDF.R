prism_data <- system.file("testdata/prism_sa.csv", package = "gDRimport")
prism_data_2 <- system.file("testdata/prism_sa_2.csv", package = "gDRimport")
prism_data_3 <- system.file("testdata/prism_combo.csv", package = "gDRimport")
prism_data_path <- system.file("testdata/prism_collapsed_LOGFC.csv", package = "gDRimport")
cell_line_data_path <- system.file("testdata/prism_cell_lines.csv", package = "gDRimport")
treatment_data_path <- system.file("testdata/prism_treatment.csv", package = "gDRimport")
prism_meta <- system.file("testdata/prism_model.csv", package = "gDRimport")

test_that("prism level5 single-agent data can be processed into gDR input format ", {
  df_prism <- purrr::quietly(convert_LEVEL5_prism_to_gDR_input)(prism_data, prism_meta)
  expect_is(df_prism$result, "data.table")
  expect_equal(dim(df_prism$result), c(4, 12))
  expect_equal(names(df_prism$result), c("clid", "CellLineName", "Tissue", "parental_identifier", "subtype",
                                         "ReferenceDivisionTime", "Duration", "ReadoutValue", "BackgroundValue",
                                         "Gnumber", "Concentration", "masked"))
  expect_equal(df_prism$result$Duration, c(120, 240, 120, 240))
  expect_true(all(gDRutils::get_env_identifiers(c("drug", "cellline"),
                                                simplify = FALSE) %in% names(df_prism$result)))

  # testing format of clid, CellLineName and Tissue column
  expect_equal(df_prism$result$clid, rep("some_clid", 4))
  expect_equal(
    df_prism$result$Tissue,
    rep("Breast", 4))

  df_prism_unknown <- purrr::quietly(convert_LEVEL5_prism_to_gDR_input)(prism_data_2, prism_meta)
  expect_is(df_prism_unknown$result, "data.table")
  expect_equal(dim(df_prism_unknown$result), c(4, 12))

  # testing format of clid, CellLineName and Tissue column
  expect_equal(df_prism_unknown$result$clid, df_prism_unknown$result$CellLineName)
  expect_equal(df_prism_unknown$result$Tissue, rep("unknown", 4))
})

test_that("prism level5 combo data can be processed into gDR input format ", {
  df_prism <- purrr::quietly(convert_LEVEL5_prism_to_gDR_input)(prism_data_3, prism_meta)
  expect_is(df_prism$result, "data.table")
  expect_equal(dim(df_prism$result), c(2, 14))
  expect_equal(names(df_prism$result), c("clid", "CellLineName", "Tissue", "parental_identifier", "subtype",
                                         "ReferenceDivisionTime", "Duration", "ReadoutValue", "BackgroundValue",
                                         "Gnumber", "Gnumber_2", "Concentration", "Concentration_2", "masked"))
  expect_true(all(gDRutils::get_env_identifiers(c("drug", "drug2", "cellline"),
                                                simplify = FALSE) %in% names(df_prism$result)))

  # testing format of clid, CellLineName and Tissue column
  expect_equal(df_prism$result$clid, rep("some_clid", 2))
  expect_equal(
    df_prism$result$Tissue,
    rep("Breast", 2))
})

test_that("prism level6  data can be processed into gDR format ", {
  df_prism <- purrr::quietly(convert_LEVEL6_prism_to_gDR_input)(prism_data_path,
                                                                cell_line_data_path,
                                                                treatment_data_path,
                                                                prism_meta)
  expect_is(df_prism$result, "data.table")
  expect_equal(dim(df_prism$result), c(3, 13))
  expect_equal(names(df_prism$result), c("clid", "Gnumber", "DrugName", "drug_moa", "Duration", "Concentration",
                                         "ReadoutValue", "masked", "CellLineName", "Tissue", "parental_identifier",
                                         "subtype", "ReferenceDivisionTime"))
  expect_true(all(gDRutils::get_env_identifiers(c("drug", "cellline"),
                                                simplify = FALSE) %in% names(df_prism$result)))
  expect_true(all(is.na(df_prism$result$Duration)))
  expect_type(df_prism$result$Duration, "double")

  # testing format of clid, CellLineName and Tissue column
  expect_equal(df_prism$result$clid, df_prism$result$CellLineName)
  expect_equal(
    df_prism$result$Tissue,
    rep("Breast", 3))
})

test_that("prism level6 data in the DepMap OncRef layout can be processed into gDR format", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  data.table::fwrite(data.table::data.table(
    V1 = c("ACH-1", "ACH-2"),
    `PRC-1::0.01::P1::P1` = c(-0.1, -0.2),
    `PRC-1::0.1::P1::P1` = c(-0.5, -0.9)
  ), file.path(dir, "lfc.csv"))
  data.table::fwrite(data.table::data.table(
    depmap_id = c("ACH-1", "ACH-2"), pool_id = "P1", analyte_id = c("A1", "A2"), cellset = "PR500", screen = "S1"
  ), file.path(dir, "analytes.csv"))
  data.table::fwrite(data.table::data.table(
    CompoundName = "drug_a", SampleID = "PRC-1", CompoundPlate = "P1", Release = "OncRef 23Q2",
    GeneSymbolOfTargets = "TARGET1", TargetOrMechanism = "inhibitor of TARGET1"
  ), file.path(dir, "compounds.csv"))
  data.table::fwrite(data.table::data.table(
    ModelID = c("ACH-1", "ACH-2"), CCLEName = c("CL1_BREAST", "CL2_LUNG"), OncotreeLineage = c("Breast", "Lung")
  ), file.path(dir, "model.csv"))

  df_prism <- purrr::quietly(convert_LEVEL6_prism_to_gDR_input)(file.path(dir, "lfc.csv"),
                                                                file.path(dir, "analytes.csv"),
                                                                file.path(dir, "compounds.csv"),
                                                                file.path(dir, "model.csv"))
  trt <- df_prism$result[Gnumber != gDRutils::get_env_identifiers("untreated_tag")[1]]
  expect_equal(NROW(trt), 4)
  expect_equal(unique(trt$Gnumber), "PRC-1")
  expect_equal(unique(trt$DrugName), "drug_a")
  expect_equal(unique(trt$drug_moa), "inhibitor of TARGET1")
  expect_type(trt$Concentration, "double")
  expect_setequal(trt$Concentration, c(0.01, 0.1))
  expect_setequal(trt$clid, c("CL1_BREAST", "CL2_LUNG"))
})


# Write the given tables as csv files into a new temporary directory; returns their paths.
write_prism_files <- function(tables) {
  dir <- tempfile()
  dir.create(dir)
  vapply(names(tables), function(n) {
    path <- file.path(dir, paste0(n, ".csv"))
    data.table::fwrite(tables[[n]], path)
    path
  }, character(1))
}

prism_test_model <- function() {
  data.table::data.table(
    ModelID = c("ACH-000001", "ACH-000002", "ACH-000003"),
    CCLEName = c("CL1_BREAST", "CL2_LUNG", ""),
    OncotreeLineage = c("Breast", "Lung", NA)
  )
}

test_that("prism level6 reads the long OncRef Luminex layout", {
  paths <- write_prism_files(list(
    lfc = data.table::data.table(
      screen = "S1", CompoundPlate = "P1",
      SampleID = c("PRC-1", "PRC-1", "PRC-2", "PRC-1", "PRC-1"),
      pert_dose = c(0.01, 0.1, 1, 0.01, 0.1),
      pert_dose_unit = c("uM", "uM", "ug/mL", "uM", "uM"),
      cellset = "PR500", pool_id = "P1",
      depmap_id = c("ACH-000001", "ACH-000001", "ACH-000001", "ACH-000003", "ACH-000099"),
      LFC = c(-0.1, -1, -2, -0.5, -0.3),
      LFC_uncorrected = 0,
      outlier = c(FALSE, TRUE, FALSE, FALSE, FALSE)
    ),
    compounds = data.table::data.table(
      CompoundPlate = "P1", SampleID = c("PRC-1", "PRC-1", "PRC-2"), CompoundName = c("drug_a", "drug_a", "adc_b"),
      GeneSymbolOfTargets = "TARGET1", TargetOrMechanism = "inhibitor of TARGET1"
    ),
    model = prism_test_model()
  ))

  out <- purrr::quietly(convert_LEVEL6_prism_to_gDR_input)(paths[["lfc"]], NULL, paths[["compounds"]],
                                                           paths[["model"]])$result
  trt <- out[Gnumber != gDRutils::get_env_identifiers("untreated_tag")[1]]

  # the ug/mL compound and the cell line missing from Model.csv are left out
  expect_equal(unique(trt$Gnumber), "PRC-1")
  expect_equal(NROW(trt), 3)
  expect_equal(unique(trt$drug_moa), "inhibitor of TARGET1")
  # a model without CCLEName gets its ModelID as cell line ID
  expect_setequal(trt$clid, c("CL1_BREAST", "ACH-000003"))
  expect_equal(trt[clid == "ACH-000003", Tissue], "unknown")
  # the corrected LFC is used and outliers are kept as masked
  expect_equal(trt[clid == "CL1_BREAST" & Concentration == 0.1, ReadoutValue], 2 ^ -1)
  expect_true(trt[clid == "CL1_BREAST" & Concentration == 0.1, masked])
  expect_equal(sum(trt$masked), 1)

  corrections <- get_import_provenance(out)$corrections
  expect_equal(corrections$prism_cell_lines_unmapped, "ACH-000099")
  expect_equal(corrections$prism_compounds_dropped_by_unit, list(units = "ug/mL", compounds = "PRC-2"))
  expect_equal(corrections$prism_outliers_masked, 1)
  expect_identical(get_import_provenance(out)$source$dose_units, "uM")

  # dose_units = NULL keeps every unit
  all_units <- purrr::quietly(convert_LEVEL6_prism_to_gDR_input)(paths[["lfc"]], NULL, paths[["compounds"]],
                                                                 paths[["model"]], dose_units = NULL)$result
  expect_true("PRC-2" %in% all_units$Gnumber)
})

test_that("prism level6 reads the long OncRef sequencing layout without a cell line file", {
  paths <- write_prism_files(list(
    lfc = data.table::data.table(
      depmap_id = c("ACH-000001", "ACH-000002"), lua = "L1", pool_id = "P1", cell_set = "PR2025A",
      CompoundPlate = "P1", SampleID = "PRC-1", pert_dose = 0.5, pert_dose_unit = "uM",
      median_l2fc = c(-1, -2), median_l2fc_uncorrected = 0, num_bio_reps = 3
    ),
    compounds = data.table::data.table(
      CompoundPlate = "P1", SampleID = "PRC-1", CompoundName = "drug_a", TargetOrMechanism = "inhibitor of TARGET1"
    ),
    model = prism_test_model()
  ))

  out <- purrr::quietly(convert_LEVEL6_prism_to_gDR_input)(paths[["lfc"]], NULL, paths[["compounds"]],
                                                           paths[["model"]])$result
  trt <- out[Gnumber != gDRutils::get_env_identifiers("untreated_tag")[1]]
  expect_equal(NROW(trt), 2)
  expect_setequal(trt$clid, c("CL1_BREAST", "CL2_LUNG"))
  expect_setequal(trt$ReadoutValue, c(0.5, 0.25))
  expect_true(all(trt$Concentration == 0.5))
  expect_false(any(trt$masked))
  expect_false("cell_line_data" %in% get_import_provenance(out)$files$role)
})

test_that("prism level6 matches pool-prefixed IDs and drops cell lines that failed STR profiling", {
  paths <- write_prism_files(list(
    lfc = data.table::data.table(
      V1 = c("PR500_ACH-000001", "PR300_ACH-000002", "PR500_ACH-000002_FAILED_STR"),
      `PLATE_X1:A03::MTS005` = c(-0.1, -0.2, -0.3)
    ),
    cells = data.table::data.table(
      depmap_id = c("ACH-000001", "ACH-000002", "ACH-000002"), ccle_name = c("CL1_BREAST", "CL2_LUNG", "CL2_LUNG"),
      row_name = c("PR500_ACH-000001", "PR300_ACH-000002", "PR500_ACH-000002_FAILED_STR")
    ),
    treatment = data.table::data.table(
      column_name = "PLATE_X1:A03::MTS005", broad_id = "BRD-1", dose = 2.5, name = "drug_a", moa = "some moa"
    ),
    model = prism_test_model()
  ))

  out <- purrr::quietly(convert_LEVEL6_prism_to_gDR_input)(paths[["lfc"]], paths[["cells"]], paths[["treatment"]],
                                                           paths[["model"]])$result
  trt <- out[Gnumber != gDRutils::get_env_identifiers("untreated_tag")[1]]
  expect_setequal(trt$clid, c("CL1_BREAST", "CL2_LUNG"))
  expect_false(anyNA(trt$clid))
  expect_equal(NROW(trt), 2)
  expect_equal(unique(trt$Concentration), 2.5)
  expect_equal(get_import_provenance(out)$corrections$prism_failed_str_dropped, "PR500_ACH-000002_FAILED_STR")
})

test_that("prism level6 reads the OncRef 23Q4 compound list", {
  paths <- write_prism_files(list(
    lfc = data.table::data.table(
      V1 = c("ACH-000001", "ACH-000002"),
      `BRD-A1::0.01::PMTS064::MTS023` = c(-0.1, -0.2)
    ),
    compounds = data.table::data.table(
      `Compound name` = "drug_a", `Gene name of target(s)` = "TARGET1", `Target/ mechanism` = "inhibitor of TARGET1",
      Synonyms = "", Release = "23Q4", pert_mfc_id = "BRD-A1"
    ),
    model = prism_test_model()
  ))

  out <- purrr::quietly(convert_LEVEL6_prism_to_gDR_input)(paths[["lfc"]], NULL, paths[["compounds"]],
                                                           paths[["model"]])$result
  trt <- out[Gnumber != gDRutils::get_env_identifiers("untreated_tag")[1]]
  expect_equal(NROW(trt), 2)
  expect_equal(unique(trt$Gnumber), "BRD-A1")
  expect_equal(unique(trt$DrugName), "drug_a")
  expect_equal(unique(trt$drug_moa), "inhibitor of TARGET1")
  expect_equal(unique(trt$Concentration), 0.01)
})
