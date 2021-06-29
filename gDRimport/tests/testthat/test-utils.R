context("assert_utils")

test_that("assert_utils", {
  
  # real data
  td1 <- get_test_data1()
  ref_tbl <- read.csv2(td1$ref_t1)
  expect_equal(standardize_record_values(ref_tbl$Gnumber), ref_tbl$Gnumber)
  
  # toyset
  test_v <- c(letters[1:4], names(DICTIONARY))
  names(test_v) <- test_v
  res_v <- c(letters[1:4], as.character(DICTIONARY))
  names(res_v) <- names(test_v)
  expect_equal(standardize_record_values(test_v), res_v)
})