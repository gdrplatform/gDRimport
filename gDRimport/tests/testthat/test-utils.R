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

  expect_true(all(vapply(standardize_df(ref_tbl), is.character, logical(1))))
})


test_that("read_ref_data and write_ref_data_df works as expected", {
  testData <- data.frame(band = "Scorpions", song = "Wind of change")
  testKeys <- "Rock"
  testRowMaps <- "Best Bands Ever"
  dir <- tempdir()
  write.table(testData, file.path(dir, "ref_band.tsv"))
  yaml::write_yaml(testKeys, file.path(dir, "ref_keys.yaml"))
  yaml::write_yaml(testRowMaps, file.path(dir, "ref_row_maps.yaml"))
  data <- read_ref_data(dir)
  checkmate::expect_list(data, len = 3)
  checkmate::expect_list(write_ref_data_df(data, dir))

  td1 <- get_test_data1()
  manifest <- readxl::read_excel(td1$m_file)
  template <- readxl::read_excel(td1$t_files[[1]])
  raw <- readxl::read_excel(td1$r_files[[1]])
  data <- list(Manifest = manifest,
               Template = template,
               RawData = raw)
  save_file_type_info(data, dir)
  expect_true(file.exists(file.path(dir, "fileTypeInfo.csv")))

  testSE <- SummarizedExperiment::SummarizedExperiment()
  write_ref_data_se(testSE, dir)
  expect_true(file.exists(file.path(dir, "ref_df_raw_data.tsv")))
})
