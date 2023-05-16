context("load_files")

  test_that("load_manifest", {

  # get test_data
  td1 <- get_test_data()

  # valid output returned for "manifest.xlsx"
  m_df <- load_manifest(manifest_path(td1))
  expect_identical(td1@ref_m_df, m_df$data)

  #get test data2
  td2 <- get_test_Tecan_data()

  # valid output returned for "manifest.xlsx"
  m_df <- load_manifest(td2$m_file)
  ref_m_df <- readRDS(td2$ref_m_df)
  expect_equal(m_df, ref_m_df)

  # expected error(s) returned
  err_msg1 <- "Assertion on 'manifest_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_manifest("/non/existent_file"), err_msg1)

  err_msg2 <- "'manifest_file' must be a character vector"
  expect_error(load_manifest(c(2, 3)), err_msg2)

  err_msg3 <- "Barcodes in Manifest must be unique!"
  expect_error(load_manifest(c(manifest_path(td1), manifest_path(td1))), err_msg3)

})


test_that("load_results", {

  # get test_data
  td1 <- get_test_data()

  headers <- gDRutils::get_env_identifiers()
  headers$barcode <- headers$barcode[[1]]

  # valid output returned for the two xlsx files
  res_tbl <- load_results(df_results_files = c(result_path(td1)), headers = headers)
  ## check with reference
  ## reference obtained with: write.csv2(res_tbl,file = "ref_RawData_day0_day7_xlsx.csv",row.names = FALSE) # nolint
  ref_tbl <- data.table::fread(td1@ref_r1_r2)
  expect_equal(res_tbl, ref_tbl)

  # valid output returned for data.table input
  df_results <- data.table::data.table(datapath = result_path(td1), name = basename(result_path(td1)))
  res_df_tbl <- load_results(df_results, headers = headers)
  expect_equal(res_df_tbl, ref_tbl)

  # valid output is returned for a single xlsx file
  res_tbl2 <- load_results(df_results_files = c(result_path(td1)[1]), headers = headers)
  ## check with reference
  ref_tbl2 <- data.table::fread(td1@ref_r1)
  expect_equal(res_tbl2, ref_tbl2)

  # get test_Tecan_data
  td2 <- get_test_Tecan_data()

  # valid output returned for Tecan format
  res_tbl3 <- load_results(df_results_files = c(td2$r_files), instrument = "Tecan", headers = headers)
  ## check with reference
  ref_tbl3 <- readRDS(td2$ref_r_df)
  expect_equal(res_tbl3, ref_tbl3)

  # expected error(s) returned
  err_msg1 <- "Assertion on 'results_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_results(c(result_path(td1)[1], "/non/existent_file")), err_msg1)

  # expected error(s) returned
  err_msg2 <- "Assertion on 'instrument' failed: Must comply to pattern '^EnVision$|^long_tsv$|^Tecan$'."
  expect_error(load_results(result_path(td1), "invalid_instrument"), err_msg2, fixed = TRUE)
})


test_that("load_templates", {

  # get test_data
  td1 <- get_test_data()

  # valid output returned for the two xlsx files
  t_tbl <- load_templates(df_template_files = c(template_path(td1)))
  ## check with reference
  ref_tbl <- data.table::fread(td1@ref_t1_t2, colClasses = "character")
  expect_equal(t_tbl, ref_tbl)

  # valid output returned for data.table input
  df_templates <- data.table::data.table(datapath = template_path(td1), name = basename(template_path(td1)))
  res_t_tbl <- load_templates(df_templates)
  expect_equal(res_t_tbl, ref_tbl)

  # get test_Tecan_data
  td2 <- get_test_Tecan_data()

  # valid output is returned for xlsx files
  res_t_tbl3 <- load_templates(df_template_files = c(td2$t_files))
  ## check with reference
  ref_tbl3 <- .standardize_untreated_values(readRDS(td2$ref_t_df))
  expect_equal(res_t_tbl3, ref_tbl3)

  # expected error(s) returned
  err_msg1 <- "Assertion on 'template_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_templates(c(template_path(td1)[1], "/non/existent_file")), err_msg1)
})



test_that("load_templates returns an error when there is no untreated conditions", {
  err_msg <- "No untreated controls were found in the template. Please upload the appropriate template."
  expect_error(load_templates(system.file("extdata/data_for_unittests/Template_7daytreated.xlsx",
                                          package = "gDRimport")), err_msg)

})

test_that("load_data", {

  # get test_data
  td1 <- get_test_data()

  l_tbl <- load_data(manifest_path(td1), template_path(td1), result_path(td1))
  # valid output returned for manifest
  expect_identical(td1@ref_m_df, l_tbl$manifest)
  # valid output returned for templates
  ref_tbl <- .standardize_untreated_values(data.table::fread(td1@ref_t1_t2))
  expect_equal(l_tbl$treatments, ref_tbl)
  # valid output returned for results
  ref_tbl <- data.table::fread(td1@ref_r1_r2)
  expect_equal(l_tbl$data, ref_tbl)

  #get test_Tecan_data (Tecan format)
  td2 <- get_test_Tecan_data()

  l_tbl2 <- load_data(td2$m_file, td2$t_files, td2$r_files, instrument = "Tecan")
  # valid output returned for manifest
  ref_m_df <- readRDS(td2$ref_m_df)
  expect_equal(ref_m_df$data, l_tbl2$manifest)
  # valid output returned for templates
  ref_t_df <- .standardize_untreated_values(readRDS(td2$ref_t_df))
  expect_equal(ref_t_df, l_tbl2$treatments)
  # valid output returned for results
  ref_r_df <- readRDS(td2$ref_r_df)
  expect_equal(l_tbl2$data, ref_r_df)
  
  # get test_EnVision_data (EnVision format)
  td4 <- get_test_EnVision_data()
  l_tbl4 <-
    load_data(td4$m_file, td4$t_files, td4$r_files, instrument = "EnVision")
  ref_l <- readRDS(td4$ref_l_path)
  # valid output returned for manifest
  expect_equal(ref_l$manifest, l_tbl4$manifest)
  # valid output returned for templates
  expect_equal(ref_l$treatments, l_tbl4$treatments)
  # valid output returned for results
  expect_equal(ref_l$data, l_tbl4$data)
  
  # expected error(s) returned - manifest
  err_msg1 <- "'manifest_file' must be a readable path"
  expect_error(load_data("/non/existent_file", template_path(td1), result_path(td1)), err_msg1)

  err_msg2 <- "'manifest_file' must be a character vector"
  expect_error(load_data(c(2, 3), template_path(td1), result_path(td1)), err_msg2)

  err_msg3 <- "Barcodes in Manifest must be unique!"
  expect_error(load_manifest(c(manifest_path(td1), manifest_path(td1))), err_msg3)

  # expected error(s) returned - templates
  err_msg4 <- "Following path(s) with no read permission found: '/non/existent_file'"
  expect_error(load_data(manifest_path(td1), c(result_path(td1)[1], "/non/existent_file"), 
                         result_path(td1)), err_msg4, fixed = TRUE)

  # expected error(s) returned
  err_msg5 <- "Assertion on 'instrument' failed: Must comply to pattern '^EnVision$|^long_tsv$|^Tecan$'."
  expect_error(load_data(manifest_path(td1), template_path(td1), result_path(td1), "invalid_instrument"), 
               err_msg5, fixed = TRUE)

  # expected error(s) returned - results
  err_msg6 <- "Assertion on 'results_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_data(manifest_path(td1), template_path(td1), c(result_path(td1)[1], "/non/existent_file")), 
               err_msg6)

})

test_that(".get_plate_size works as expected", {
  df <- read_excel_to_dt(system.file("extdata/data1/RawData_day7.xlsx", package = "gDRimport"))
  size <- .get_plate_size(df)
  expect_length(size, 2)
  expect_equal(prod(size), 384)
})

test_that(".check_file_structure works as expected", {
  df <- read_excel_to_dt(system.file("extdata/data1/RawData_day7.xlsx", package = "gDRimport"))
  df <-
    df[, !apply(df[seq_len(35), ], 2, function(x) {
      all(is.na(x))}), with = FALSE]
  Bckd_info_idx <-
    which(df[[1]] %in% "Background information")
  if (length(Bckd_info_idx) > 0) {
    df[Bckd_info_idx + 1, 1] <- df[Bckd_info_idx, 1]
    df[Bckd_info_idx, 1] <- ""
  }
  size <- .get_plate_size(df)
  n_row <- size[1]
  n_col <- size[2]
  df_to_check <- df[, c(-6:-1), with = FALSE]
  ml <- lapply(df_to_check, function(x) {
    is.na(suppressWarnings(as.numeric(x)))
  })
  full_rows <- rowSums(do.call(cbind, ml)) != ncol(df_to_check)
  plate_row <- which(df[[1]] %in% "Plate information")
  spacer_rows <- unlist(lapply(plate_row, function(x) c(x + 1, x + 2, x + 4 + n_row)))
  data_rows <- unlist(lapply(plate_row, function(x) (x + 4):(x + 4 + n_row - 1)))
  #fill up data_rows
  df[data_rows, ] <- lapply(df[data_rows, ], function(x) ifelse(is.na(x), "0", x))
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
  expect_null(.check_file_structure(df, basename(results_filename[[iF]]), 
                                    iS, readout_offset, n_row, n_col, iB, barcode_col))

  df2 <- read_excel_to_dt(system.file("extdata/data1/RawData_day7.xlsx", package = "gDRimport"))
  expect_error(.check_file_structure(df2, basename(results_filename[[iF]]),
                                    iS, readout_offset, n_row, n_col, iB, barcode_col))
})

test_that(".fill_empty_wells works as expected", {
  df <- read_excel_to_dt(system.file("extdata/data1/RawData_day7.xlsx", package = "gDRimport"), col_names = FALSE)
  df <-
    df[, !apply(df[seq_len(35), ], 2, function(x) {
      all(is.na(x))}), with = FALSE]
  Bckd_info_idx <-
    which(df[[1]] %in% "Background information")
  if (length(Bckd_info_idx) > 0) {
    df[Bckd_info_idx + 1, 1] <- df[Bckd_info_idx, 1]
    df[Bckd_info_idx, 1] <- ""
  }
  size <- .get_plate_size(df)
  n_row <- size[1]
  n_col <- size[2]
  df_to_check <- df[, -6:-1, with = FALSE]
  ml <- lapply(df_to_check, function(x) {
    is.na(suppressWarnings(as.numeric(x)))
  })
  full_rows <- rowSums(do.call(cbind, ml)) != ncol(df_to_check)
  plate_row <- which(df[[1]] %in% "Plate information")
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

  df_test <- data.table::data.table(a = c(untreated_tags[[1]], untreated_tags[[2]],
                              toupper(untreated_tags[[1]]), tolower(untreated_tags[[2]])))
  df_corrected <- .standardize_untreated_values(df_test)
  expect_true(all(unlist(df_corrected) == untreated_tags[[1]]))
})

test_that("check_metadata_names works as expected", {
  
  td1 <- get_test_data()
  m_file <- manifest_path(td1)
  m_data <- read_excel_to_dt(m_file)
  
  # default
  result <- check_metadata_names(col_df = colnames(m_data))
  expect_equal(result, colnames(m_data))
  
  # tests manifest
  result <- check_metadata_names(col_df = colnames(m_data), m_file, df_type = "manifest")
  expect_equal(result, colnames(m_data))
  
  # tests template
  t_file <- template_path(td1)[[1]]
  t_data <- correct_template_sheets(t_file)
  result <- check_metadata_names(col_df = t_data[[1]])
  expect_equal(result, t_data[[1]])
  result <- check_metadata_names(col_df = t_data[[1]], df_type = "template")
  expect_equal(result, t_data[[1]])
  result <- check_metadata_names(col_df = t_data[[1]], df_type = "template_treatment")
  expect_equal(result, t_data[[1]])
  
  # missing gnumber
  expect_error(check_metadata_names(col_df = t_data[[1]][-1], df_type = "template_treatment"))
  # missing gnumber2
  expect_error(check_metadata_names(col_df = t_data[[1]][-3], df_type = "template_treatment"), "Template file")
  
  # names with spaces
  t_data_with_space <- t_data[[1]]
  t_data_with_space[1] <- "Gnumber "
  expect_no_error(check_metadata_names(col_df = t_data_with_space))
  # TODO add - expect futile.logger::flog.warn 
  
  # names with numbers
  t_data_with_number <- t_data[[1]]
  t_data_with_number[5] <- "1"
  expect_error(
    check_metadata_names(col_df = t_data_with_number), 
    "cannot contain special characters or start with a number"
  )
  
  # names with duplication
  t_data_with_duplication <- t_data[[1]]
  t_data_with_duplication[5] <- "Gnumber"
  t_data_with_duplication[6] <- "GnumbeR"
  expect_no_error(check_metadata_names(col_df = t_data_with_duplication))
  # TODO add - expect futile.logger::flog.warn 
  
  
  # names with resticted name
  t_data_with_resticted_name <- t_data[[1]]
  t_data_with_resticted_name[5] <- "Tissue"
  expect_error(check_metadata_names(col_df = t_data_with_resticted_name), "Metadata field name")
  
  
  # Check asserts 
  expect_error(check_metadata_names(col_df = NULL), "Assertion on 'col_df' failed")
  expect_error(check_metadata_names(col_df = colnames(m_data), df_name = NULL), "Assertion on 'df_name'")
  expect_error(check_metadata_names(col_df = colnames(m_data), df_type = 5), "Assertion on 'df_type'")
  
  # TODO: add asserts on check_metadata_names - df_type - current error message `object 
  # 'expected_headers' not found` is misleading
  
  #nolint start
  # expect_error(
  #   check_metadata_names(col_df = colnames(m_data), m_file, df_type = "I like pancakes"),
  #   regexp = "df_type"
  # )
  #nolint end
  
})

