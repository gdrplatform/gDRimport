prism_data <- system.file("testdata/prism_sa.csv", package = "gDRimport")
prism_data2 <- system.file("testdata/prism_combo.csv", package = "gDRimport")
prism_data_path <- system.file("testdata/prism_collapsed_LOGFC.csv", package = "gDRimport")
cell_line_data_path <- system.file("testdata/prism_cell_lines.csv", package = "gDRimport")
treatment_data_path <- system.file("testdata/prism_treatment.csv", package = "gDRimport")

test_that("prism level5 single-agent data can be processed into gDR input format ", {
  df_prism <- purrr::quietly(convert_LEVEL5_prism_to_gDR_input)(prism_data)
  expect_is(df_prism$result, "data.table")
  expect_equal(dim(df_prism$result), c(4, 7))
  expect_equal(df_prism$result$Duration, c(120, 240, 120, 240))
  expect_true(all(gDRutils::get_env_identifiers(c("drug", "cellline"),
                                                simplify = FALSE) %in% names(df_prism$result)))
})

test_that("prism level5 combo data can be processed into gDR input format ", {
  df_prism <- purrr::quietly(convert_LEVEL5_prism_to_gDR_input)(prism_data2)
  expect_is(df_prism$result, "data.table")
  expect_equal(dim(df_prism$result), c(2, 9))
  expect_true(all(gDRutils::get_env_identifiers(c("drug", "drug2", "cellline"),
                                                simplify = FALSE) %in% names(df_prism$result)))
})

test_that("prism level6  data can be processed into gDR format ", {
  df_prism <- purrr::quietly(convert_LEVEL6_prism_to_gDR_input)(prism_data_path,
                                                                cell_line_data_path,
                                                                treatment_data_path)
  expect_is(df_prism$result, "data.table")
  expect_equal(dim(df_prism$result), c(3, 8))
  expect_true(all(gDRutils::get_env_identifiers(c("drug", "cellline"),
                                                simplify = FALSE) %in% names(df_prism$result)))
  expect_equal(df_prism[["result"]][[gDRutils::get_env_identifiers("drug")]],
               c("some_drug_name_run1", "some_drug_name_run2", "vehicle"))
  
})

