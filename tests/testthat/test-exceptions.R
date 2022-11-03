library(testthat)

test_that("check get_exception_data works correctly", {
  obs <- gDRimport::get_exception_data(1)
  exp <- tibble::tribble(
    ~status_code, ~title, ~sprintf_text, ~type, ~input_type,
    "1", "Error loading manifest", "There were errors loading manifest. See logs:\n%s", "error", "manifest"
  )
  expect_equal(obs, exp)
  expect_named(obs, names(exp))
  expect_equal(dim(gDRimport::get_exception_data()), c(32, 5))

  expect_error(gDRimport::get_exception_data("two"),
               "Assertion on 'status_code' failed: Must be of type 'number' (or 'NULL'), not 'character'.",
               fixed = TRUE)
  expect_error(gDRimport::get_exception_data(0),
               paste0("Assertion on 'toString(status_code)' failed: Must be element of set",
                      " {'1','2','3','4','5','6','7','8','9','10','11','12','13','14',",
                      "'15','16','17','18','19','20','21','22','23','24','25','26',",
                      "'27','28','29','30','31','32'}, but is '0'."),
               fixed = TRUE)
})
