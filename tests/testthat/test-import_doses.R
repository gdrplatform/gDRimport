context("import_doses")

test_that("parse_D300", {
  
  # get test_data3
  td3 <- get_test_data3()
  
  # valid output returned for the D300 96 well plate example
  dose_df <- parse_D300(td3$d300_96w_file)
  ref_dose_df <- readRDS(td3$ref_d300_96w_file)
  expect_identical(dose_df, ref_dose_df)
  
  # valid output returned for the D300 348 well plate example
  dose_df <- parse_D300(td3$d300_384w_file)
  ref_dose_df <- readRDS(td3$ref_d300_384w_file)
  expect_identical(dose_df, ref_dose_df)
  
  # expected error(s) returned
  err_msg1 <- "'D300_file' must be a readable path"
  expect_error(parse_D300("/non/existent_file"), err_msg1)
  
})