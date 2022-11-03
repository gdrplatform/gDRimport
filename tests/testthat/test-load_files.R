context("load_files")

  test_that("load_manifest", {

  # get test_data1
  td1 <- get_test_data1()

  # valid output returned for "manifest.xlsx"
  m_df <- load_manifest(td1$m_file)
  expect_identical(td1$ref_m_df, m_df$data)

  #get test data2
  td2 <- get_test_data2()

  # valid output returned for "manifest.xlsx"
  m_df <- load_manifest(td2$m_file)
  ref_m_df <- readRDS(td2$ref_m_df)
  expect_identical(m_df, ref_m_df)

  # expected error(s) returned
  err_msg1 <- "Assertion on 'manifest_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_manifest("/non/existent_file"), err_msg1)

  err_msg2 <- "'manifest_file' must be a character vector"
  expect_error(load_manifest(c(2, 3)), err_msg2)

  err_msg3 <- "Barcodes in Manifest must be unique!"
  expect_error(load_manifest(c(td1$m_file, td1$m_file)), err_msg3)

})


test_that("load_results", {

  # get test_data1
  td1 <- get_test_data1()

  headers <- gDRutils::get_env_identifiers()
  headers$barcode <- headers$barcode[[1]]

  # valid output returned for the two xlsx files
  res_tbl <- load_results(df_results_files = c(td1$r_files), headers = headers)
  ## check with reference
  ## reference obtained with: write.csv2(res_tbl,file = "ref_RawData_day0_day7_xlsx.csv",row.names = FALSE) # nolint
  ref_tbl <- read.csv2(td1$ref_r1_r2)
  expect_equal(res_tbl, ref_tbl)

  # valid output returned for data.frame input
  df_results <- data.frame(datapath = td1$r_files, name = basename(td1$r_files))
  res_df_tbl <- load_results(df_results, headers = headers)
  expect_equal(res_df_tbl, ref_tbl)

  # valid output is returned for a single xlsx file
  res_tbl2 <- load_results(df_results_files = c(td1$r_files[1]), headers = headers)
  ## check with reference
  ref_tbl2 <- read.csv2(td1$ref_r1)
  expect_equal(res_tbl2, ref_tbl2)

  # get test_data2
  td2 <- get_test_data2()

  # valid output returned for Tecan format
  res_tbl3 <- load_results(df_results_files = c(td2$r_files), instrument = "Tecan", headers = headers)
  ## check with reference
  ref_tbl3 <- readRDS(td2$ref_r_df)
  expect_equal(res_tbl3, ref_tbl3)

  # expected error(s) returned
  err_msg1 <- "Assertion on 'results_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_results(c(td1$r_files[1], "/non/existent_file")), err_msg1)

  # expected error(s) returned
  err_msg2 <- "Assertion on 'instrument' failed: Must comply to pattern '^EnVision$|^long_tsv$|^Tecan$'."
  expect_error(load_results(td1$r_files, "invalid_instrument"), err_msg2, fixed = TRUE)
})


test_that("load_templates", {

  # get test_data1
  td1 <- get_test_data1()

  # valid output returned for the two xlsx files
  t_tbl <- load_templates(df_template_files = c(td1$t_files))
  ## check with reference
  ## reference obtained with: write.csv2(t_tbl,file = "ref_template_treated_untreated_xlsx.csv",row.names = FALSE) # nolint
  ref_tbl <- .standardize_untreated_values(read.csv2(td1$ref_t1_t2))
  expect_equal(standardize_df(t_tbl), standardize_df(ref_tbl))

  # valid output returned for data.frame input
  df_templates <- data.frame(datapath = td1$t_files, name = basename(td1$t_files))
  res_t_tbl <- load_templates(df_templates)
  expect_equal(standardize_df(res_t_tbl), standardize_df(ref_tbl))

  # get test_data2
  td2 <- get_test_data2()

  # valid output is returned for xlsx files
  res_t_tbl3 <- load_templates(df_template_files = c(td2$t_files))
  ## check with reference
  ref_tbl3 <- .standardize_untreated_values(readRDS(td2$ref_t_df))
  expect_equal(standardize_df(res_t_tbl3), standardize_df(ref_tbl3))

  # expected error(s) returned
  err_msg1 <- "Assertion on 'template_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_templates(c(td1$t_files[1], "/non/existent_file")), err_msg1)
})



test_that("load_templates returns an error when there is no untreated conditions", {
  err_msg <- "No untreated controls were found in the template. Please upload the appropriate template."
  expect_error(load_templates(system.file("extdata/data_for_unittests/Template_7daytreated.xlsx",
                                          package = "gDRimport")), err_msg)

})

test_that("load_data", {

  # get test_data1
  td1 <- get_test_data1()

  l_tbl <- load_data(td1$m_file, td1$t_files, td1$r_files)
  # valid output returned for manifest
  expect_identical(td1$ref_m_df, l_tbl$manifest)
  # valid output returned for templates
  ref_tbl <- .standardize_untreated_values(read.csv2(td1$ref_t1_t2))
  expect_equal(standardize_df(l_tbl$treatments), standardize_df(ref_tbl))
  # valid output returned for results
  ref_tbl <- read.csv2(td1$ref_r1_r2)
  expect_equal(l_tbl$data, ref_tbl)

  #get test_data2
  td2 <- get_test_data2()

  l_tbl2 <- load_data(td2$m_file, td2$t_files, td2$r_files, instrument = "Tecan")
  # valid output returned for manifest
  ref_m_df <- readRDS(td2$ref_m_df)
  expect_identical(ref_m_df$data, l_tbl2$manifest)
  # valid output returned for templates
  ref_t_df <- .standardize_untreated_values(readRDS(td2$ref_t_df))
  expect_equal(standardize_df(ref_t_df), standardize_df(l_tbl2$treatments))
  # valid output returned for results
  ref_r_df <- readRDS(td2$ref_r_df)
  expect_equal(l_tbl2$data, ref_r_df)

  # expected error(s) returned - manifest
  err_msg1 <- "'manifest_file' must be a readable path"
  expect_error(load_data("/non/existent_file", td1$t_files, td1$r_files), err_msg1)

  err_msg2 <- "'manifest_file' must be a character vector"
  expect_error(load_data(c(2, 3), td1$t_files, td1$r_files), err_msg2)

  err_msg3 <- "Barcodes in Manifest must be unique!"
  expect_error(load_manifest(c(td1$m_file, td1$m_file)), err_msg3)

  # expected error(s) returned - templates
  err_msg4 <- "Following path(s) with no read permission found: '/non/existent_file'"
  expect_error(load_data(td1$m_file, c(td1$r_files[1], "/non/existent_file"), td1$r_files), err_msg4, fixed = TRUE)

  # expected error(s) returned
  err_msg5 <- "Assertion on 'instrument' failed: Must comply to pattern '^EnVision$|^long_tsv$|^Tecan$'."
  expect_error(load_data(td1$m_file, td1$t_files, td1$r_files, "invalid_instrument"), err_msg5, fixed = TRUE)

  # expected error(s) returned - results
  err_msg6 <- "Assertion on 'results_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_data(td1$m_file, td1$t_files, c(td1$r_files[1], "/non/existent_file")), err_msg6)

})

test_that(".get_plate_size works as expected", {
  df <- readxl::read_excel(system.file("extdata/data1/RawData_day7.xlsx", package = "gDRimport"))
  size <- .get_plate_size(df)
  expect_length(size, 2)
  expect_equal(prod(size), 384)
})

test_that(".check_file_structure works as expected", {
  df <- readxl::read_excel(system.file("extdata/data1/RawData_day7.xlsx", package = "gDRimport"))
  df <-
    df[, !apply(df[1:35, ], 2, function(x)
      all(is.na(x)))]
  Bckd_info_idx <-
    which(as.data.frame(df)[, 1] %in% "Background information")
  if (length(Bckd_info_idx) > 0) {
    df[Bckd_info_idx + 1, 1] <- df[Bckd_info_idx, 1]
    df[Bckd_info_idx, 1] <- ""
  }
  size <- .get_plate_size(df)
  n_row <- size[1]
  n_col <- size[2]
  df_to_check <- df[, -6:-1]
  full_rows <- rowSums(as.data.frame(lapply(df_to_check, function(x) {
    is.na(suppressWarnings(as.numeric(x)))
  }))) != ncol(df_to_check)
  plate_row <- which(as.data.frame(df)[, 1] %in% "Plate information")
  spacer_rows <- unlist(lapply(plate_row, function(x) c(x + 1, x + 2, x + 4 + n_row)))
  data_rows <- unlist(lapply(plate_row, function(x) (x + 4):(x + 4 + n_row - 1)))
  #fill up data_rows
  for (i in data_rows) {
    df[i, c(is.na(df[i, ]))] <- "0"
  }
  full_rows_index <- sort(union(spacer_rows, data_rows))
  gaps <-
    min(which(full_rows)[(diff(which(full_rows)) > 20)] + 1, dim(df)[1])
  df <-
    df[full_rows_index[full_rows_index <= gaps], ] # remove extra rows
  if (ncol(df) < n_col) {
    df[, (ncol(df) + 1):n_col] <- NA
  }
  iB <- 1
  iF <- 1
  iS <- 1
  results_filename <- "data"
  ref_bckgrd <- 4
  readout_offset <- 1 + ref_bckgrd
  barcode_col <- 3
  expect_null(.check_file_structure(df, iB, iF, iS, results_filename,
                        readout_offset, n_row, n_col, barcode_col))

  df2 <- readxl::read_excel(system.file("extdata/data1/RawData_day7.xlsx", package = "gDRimport"))
  expect_error(.check_file_structure(df2, iB, iF, iS, results_filename,
                                    readout_offset, n_row, n_col, barcode_col))
})

test_that(".fill_empty_wells works as expected", {
  df <- readxl::read_excel(system.file("extdata/data1/RawData_day7.xlsx", package = "gDRimport"), col_names = FALSE)
  df <-
    df[, !apply(df[1:35, ], 2, function(x)
      all(is.na(x)))]
  Bckd_info_idx <-
    which(as.data.frame(df)[, 1] %in% "Background information")
  if (length(Bckd_info_idx) > 0) {
    df[Bckd_info_idx + 1, 1] <- df[Bckd_info_idx, 1]
    df[Bckd_info_idx, 1] <- ""
  }
  size <- .get_plate_size(df)
  n_row <- size[1]
  n_col <- size[2]
  df_to_check <- df[, -6:-1]
  full_rows <- rowSums(as.data.frame(lapply(df_to_check, function(x) {
    is.na(suppressWarnings(as.numeric(x)))
  }))) != ncol(df_to_check)
  plate_row <- which(as.data.frame(df)[, 1] %in% "Plate information")
  spacer_rows <- unlist(lapply(plate_row, function(x) c(x + 1, x + 2, x + 4 + n_row)))
  data_rows <- unlist(lapply(plate_row, function(x) (x + 4):(x + 4 + n_row - 1)))
  df2 <- .fill_empty_wells(df, plate_row, data_rows, n_row, n_col)
  expect_identical(df, df2)
  df_modified <- df
  df_modified[9, ] <- NA
  df_modified <- .fill_empty_wells(df_modified, plate_row, data_rows, n_row, n_col)
  expect_true(all(df_modified[9, ] == 0))
})


test_that(".standardize_untreated_values works as expected", {
  untreated_tags <- gDRutils::get_env_identifiers("untreated_tag")

  df_test <- data.frame(a = c(untreated_tags[[1]], untreated_tags[[2]],
                              toupper(untreated_tags[[1]]), tolower(untreated_tags[[2]])))
  df_corrected <- .standardize_untreated_values(df_test)
  expect_true(all(unlist(df_corrected) == untreated_tags[[1]]))
})
