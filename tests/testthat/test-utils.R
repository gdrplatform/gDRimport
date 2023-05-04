context("assert_utils")

test_that("assert_utils", {

  # real data
  td1 <- get_test_data()
  ref_tbl <- read.csv2(td1@ref_t1)
  expect_equal(standardize_record_values(ref_tbl$Gnumber), ref_tbl$Gnumber)

  # toyset
  test_v <- c(letters[seq_len(4)], names(DICTIONARY))
  names(test_v) <- test_v
  res_v <- c(letters[seq_len(4)], as.character(DICTIONARY))
  names(res_v) <- names(test_v)
  expect_equal(standardize_record_values(test_v), res_v)
})

test_that("detect_file_format works as expected", {
  envision_path <- list.files(system.file(package = "gDRimport", "extdata", "data1"),
                           "^RawData", full.names = TRUE)
  expect_equal(unique(unlist(lapply(envision_path, detect_file_format))), "EnVision")
  
  tecan_path <- list.files(system.file(package = "gDRimport", "extdata", "data2"),
                              "^RawData", full.names = TRUE)
  expect_equal(unique(unlist(lapply(tecan_path, detect_file_format))), "Tecan")
  
  tsv_path <- list.files(system.file(package = "gDRimport", "extdata", "data1"),
                           "ref_RawData", full.names = TRUE)
  expect_equal(unique(unlist(lapply(tsv_path, detect_file_format))), "long_tsv")
})

test_that("read_excel_to_dt works as expected", {
  datasets <- readxl::readxl_example("datasets.xlsx")
  dt <- read_excel_to_dt(datasets)
  expect_s3_class(dt, "data.table")
})
