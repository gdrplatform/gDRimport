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
  
  # testing format of clid, CellLineName and Tissue column
  expect_equal(df_prism$result$clid, df_prism$result$CellLineName)
  expect_equal(
    df_prism$result$Tissue,
    rep("Breast", 3))
})
