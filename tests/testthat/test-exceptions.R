library(testthat)

test_that("check get_exception_data works correctly", {
  obs <- gDRimport::get_exception_data(1)
  expect_equal(nrow(obs), 1)
  expect_equal(colnames(obs),
               c("status_code", "title", "sprintf_text", "type", "input_type"))
  
  expect_true(all(dim(gDRimport::get_exception_data()) >= 5))
  
  expect_error(
    gDRimport::get_exception_data("two"),
    "Assertion on 'status_code' failed: Must be of type 'number'",
    fixed = TRUE
  )
  
  expect_error(
    gDRimport::get_exception_data(0),
    paste0(
      "Assertion on \\'toString\\(status_code\\)\\' failed:.+but is \\'0\\'"
    )
  )
})
