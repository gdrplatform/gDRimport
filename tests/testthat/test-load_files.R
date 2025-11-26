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
  ref_m_df <- qs::qread(td2$ref_m_df)
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
  ref_tbl3 <- qs::qread(td2$ref_r_df)
  expect_equal(res_tbl3, ref_tbl3)

  # expected error(s) returned
  err_msg1 <- "Assertion on 'results_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_results(c(result_path(td1)[1], "/non/existent_file")), err_msg1)

  # expected error(s) returned
  err_msg2 <-
    "Assertion on 'instrument' failed: Must comply to pattern '^EnVision$|^long_tsv$|^Tecan$|^EnVision_new$|^Incucyte$'."
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
  ref_tbl3 <- .standardize_untreated_values(qs::qread(td2$ref_t_df))
  expect_equal(res_t_tbl3, ref_tbl3)

  # expected error(s) returned
  err_msg1 <- "Assertion on 'template_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_templates(c(template_path(td1)[1], "/non/existent_file")), err_msg1)
})


test_that("load_templates returns an error when there is no untreated conditions", {
  err_msg <- "No untreated controls were found in the treatment. Please upload the appropriate treatment."
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
  ref_m_df <- qs::qread(td2$ref_m_df)
  expect_equal(ref_m_df$data, l_tbl2$manifest)
  # valid output returned for templates
  ref_t_df <- .standardize_untreated_values(qs::qread(td2$ref_t_df))
  expect_equal(ref_t_df, l_tbl2$treatments)
  # valid output returned for results
  ref_r_df <- qs::qread(td2$ref_r_df)
  expect_equal(l_tbl2$data, ref_r_df)
  
  # get test_EnVision_data (EnVision format)
  td4 <- get_test_EnVision_data()
  l_tbl4 <-
    load_data(td4$m_file, td4$t_files, td4$r_files, instrument = "EnVision")
  ref_l <- qs::qread(td4$ref_l_path)
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
  err_msg5 <-
    "Assertion on 'instrument' failed: Must comply to pattern '^EnVision$|^long_tsv$|^Tecan$|^EnVision_new$|^Incucyte$'."
  expect_error(load_data(manifest_path(td1), template_path(td1), result_path(td1), "invalid_instrument"), 
               err_msg5, fixed = TRUE)

  # expected error(s) returned - results
  err_msg6 <- "Assertion on 'results_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_data(manifest_path(td1), template_path(td1), c(result_path(td1)[1], "/non/existent_file")), 
               err_msg6)
  
  
  
  # get test_tsv_data (tsv format)
  td5 <- get_test_tsv_data()
  l_tbl5 <-
    load_data(td5$m_file, td5$t_files, td5$r_files)
  ref_l <- qs::qread(td5$ref_l_path)
  # valid output returned for manifest
  expect_equal(ref_l$manifest, l_tbl5$manifest)
  # valid output returned for templates
  expect_equal(ref_l$treatments, l_tbl5$treatments)
  # valid output returned for results
  expect_equal(ref_l$data, l_tbl5$data)
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
  expect_error(check_metadata_names(col_df = t_data[[1]][-3], df_type = "template_treatment"), "Treatment file")
  
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

test_that("load_results_Incucyte works as expected", {
  # --- Test Setup ---
  # This setup runs once when the file is sourced by testthat.
  
  # Define standard headers to pass to the function
  headers <- gDRutils::get_env_identifiers()
  bcode_name <- headers$barcode[1]
  d_name <- headers$duration
  well_rname <- headers$well_position[1]
  well_cname <- headers$well_position[2]
  
  # Define paths for temporary test files
  file_csv_1_path <- tempfile(fileext = ".csv")
  file_csv_2_na_path <- tempfile(fileext = ".csv")
  file_tsv_3_path <- tempfile(fileext = ".tsv")
  file_xlsx_4_path <- tempfile(fileext = ".xlsx")
  file_bad_header_path <- tempfile(fileext = ".csv")
  file_bad_barcode_path <- tempfile(fileext = ".csv")
  file_custom_header_path <- tempfile(fileext = ".csv")
  
  # Content for File 1 (CSV)
  # Barcode on line 4, Data starts on line 7 (dstart_idx = 7)
  writeLines(
    c(
      "Vessel Name: Test Plate 1,,,",
      "Metric: Red Object Count,,,",
      "Notes:,,,",
      sprintf("Barcode,%s", "PLATE_001_CSV,,"),
      "Some extra metadata line,,,",
      "Another metadata line,,,",
      "Date Time,Elapsed,A1,A2",
      "8/18/25 3:02,0,100,200",
      "8/18/25 9:02,6,110,210"
    ),
    file_csv_1_path
  )
  
  # Content for File 2 (CSV with NAs)
  # Barcode on line 2, Data starts on line 4 (dstart_idx = 4)
  writeLines(
    c(
      "Vessel Name: Test Plate 2,,,",
      sprintf("Barcode,%s", "PLATE_002_CSV_NA,,"),
      "Cell Type: MCF-7,,,",
      "Date Time,Elapsed,B1,B2",
      "8/18/25 3:02,0,300,400",
      "8/18/25 9:02,6,310,410",
      "8/18/25 15:02,12,320,NA",
      # This NA ReadoutValue should be dropped
      "8/18/25 21:02,NA,330,430" # This NA Elapsed should drop 2 rows
    ),
    file_csv_2_na_path
  )
  
  # Content for File 3 (TSV)
  # Barcode on line 2, Data starts on line 4 (dstart_idx = 4)
  writeLines(c(
    paste("Vessel Name\tTest Plate 3\t\t"),
    paste("Barcode\tPLATE_003_TSV\t\t"),
    paste("Cell Type\tA549\t\t"),
    paste("Date Time", "Elapsed", "C1", "C2", sep = "\t"),
    paste("8/18/25 3:02", "0", "500", "600", sep = "\t"),
    paste("8/18/25 9:02", "6", "510", "610", sep = "\t")
  ),
  file_tsv_3_path)
  
  # Content for File 4 (XLSX)
  # Barcode on line 2, Data starts on line 4 (dstart_idx = 4)
  xlsx_df <- data.frame(
    V1 = c(
      "Vessel Name",
      "Barcode",
      NA,
      "Date Time",
      "8/18/25 3:02",
      "8/18/25 9:02"
    ),
    V2 = c("Test Plate 4", "PLATE_004_XLSX", NA, "Elapsed", "0", "6"),
    V3 = c(NA, NA, NA, "D1", "700", "710"),
    V4 = c(NA, NA, NA, "D2", "800", "810"),
    stringsAsFactors = FALSE
  )
  # Write to .xlsx file without column names
  openxlsx::write.xlsx(xlsx_df, file_xlsx_4_path, colNames = FALSE)
  
  
  # Content for Bad Header File (Missing 'Date Time')
  writeLines(
    c(
      "Vessel Name: Bad Plate,,",
      sprintf("Barcode,%s", "PLATE_005_BAD,"),
      "Elapsed,A1,A2",
      "0,100,200"
    ),
    file_bad_header_path
  )
  
  # Content for Bad Barcode File (Missing Barcode)
  writeLines(
    c(
      "Vessel Name: Bad Barcode Plate,,,",
      "Notes: No barcode line,,,",
      "Date Time,Elapsed,A1,A2",
      "8/18/25,0,100,200"
    ),
    file_bad_barcode_path
  )
  
  # Content for Custom Header Test
  writeLines(
    c(
      "Vessel,PLATE_CUSTOM",
      # Using 'Vessel' as barcode header
      "Notes: custom test",
      "Date Time,Elapsed,A1",
      "8/18/25 3:02,0,900",
      "8/18/25 9:02,6,950"
    ),
    file_custom_header_path
  )
  
  # Clean up all temporary files when tests are done
  on.exit(unlink(
    c(
      file_csv_1_path,
      file_csv_2_na_path,
      file_tsv_3_path,
      file_xlsx_4_path,
      file_bad_header_path,
      file_bad_barcode_path,
      file_custom_header_path
    )
  ))
  # --- End Setup ---
  
  
  # (1) Test: single CSV file
  dt_csv <- load_results_Incucyte(file_csv_1_path, headers)
  
  expect_s3_class(dt_csv, "data.table")
  # 2 wells * 2 timepoints = 4 rows
  expect_equal(nrow(dt_csv), 4)
  
  # Check column names
  expect_true(all(
    c(bcode_name, d_name, well_rname, well_cname, "ReadoutValue") %in% names(dt_csv)
  ))
  expect_false(any(c("Well", "Elapsed") %in% names(dt_csv)))
  
  # Check content
  expect_equal(unique(dt_csv[[bcode_name]]), "PLATE_001_CSV")
  expect_equal(sort(unique(dt_csv[[d_name]])), c(0, 6))
  expect_equal(sort(dt_csv$ReadoutValue), c(100, 110, 200, 210))
  expect_equal(sort(unique(dt_csv[[well_rname]])), "A")
  expect_equal(sort(unique(dt_csv[[well_cname]])), c("1", "2"))
  
  
  # (2) Test: single TSV file
  # Note: The function uses data.table::fread, which auto-detects sep.
  dt_tsv <- load_results_Incucyte(file_tsv_3_path, headers)
  
  expect_s3_class(dt_tsv, "data.table")
  # 2 wells * 2 timepoints = 4 rows
  expect_equal(nrow(dt_tsv), 4)
  
  # Check content
  expect_equal(unique(dt_tsv[[bcode_name]]), "PLATE_003_TSV")
  expect_equal(sort(dt_tsv$ReadoutValue), c(500, 510, 600, 610))
  expect_equal(sort(unique(dt_tsv[[well_rname]])), "C")
  expect_equal(sort(unique(dt_tsv[[well_cname]])), c("1", "2"))
  
  
  # (3) Test: single XLSX file
  dt_xlsx <- load_results_Incucyte(file_xlsx_4_path, headers)
  
  expect_s3_class(dt_xlsx, "data.table")
  # 2 wells * 2 timepoints = 4 rows
  expect_equal(nrow(dt_xlsx), 4)
  
  # Check content
  expect_equal(unique(dt_xlsx[[bcode_name]]), "PLATE_004_XLSX")
  expect_equal(sort(dt_xlsx$ReadoutValue), c(700, 710, 800, 810))
  expect_equal(sort(unique(dt_xlsx[[well_rname]])), "D")
  expect_equal(sort(unique(dt_xlsx[[well_cname]])), c("1", "2"))
  
  # (4) Test: multiple files (CSV, TSV, XLSX) and NA handling
  all_files <-
    c(file_csv_1_path,
      file_csv_2_na_path,
      file_tsv_3_path,
      file_xlsx_4_path)
  dt_all <- load_results_Incucyte(all_files, headers)
  
  expect_s3_class(dt_all, "data.table")
  
  # File 1 (CSV): 4 rows
  # File 2 (CSV_NA): 2 wells * 3 timepoints = 6 rows.
  #         One ReadoutValue=NA is removed (1 row).
  #         One Elapsed=NA is removed (2 rows).
  #         Total valid from File 2 = 6 - 1 - 2 = 3 rows.
  # File 3 (TSV): 4 rows
  # File 4 (XLSX): 4 rows
  # Total rows = 4 + 3 + 4 + 4 = 15
  expect_equal(nrow(dt_all), 17)
  
  # Check content
  expect_equal(
    sort(unique(dt_all[[bcode_name]])),
    c(
      "PLATE_001_CSV",
      "PLATE_002_CSV_NA",
      "PLATE_003_TSV",
      "PLATE_004_XLSX"
    )
  )
  
  # Check that NA rows were truly dropped from all files
  expect_false(any(is.na(dt_all$ReadoutValue)))
  expect_false(any(is.na(dt_all[[d_name]])))
  
  
  # (5) Test: Error conditions
  
  # Missing file (relies on the tryCatch inside the function)
  missing_file <- "/non/existent/file.csv"
  expect_error(
    load_results_Incucyte(missing_file, headers),
    "Error reading /non/existent/file.csv"
  )
  
  # Bad header (missing 'Date Time')
  # This will cause dstart_idx to be integer(0), which fails on subsetting
  expect_error(
    load_results_Incucyte(file_bad_header_path, headers),
    "Invalid header in the result file: (missing 'Data Time' column)",
    fixed = TRUE
  )
  
  # Bad header (missing barcode)
  # This will cause barcode_idx to be integer(0), which fails on subsetting
  expect_error(
    load_results_Incucyte(file_bad_barcode_path, headers),
    "Invalid header in the result file: (missing 'Barcode' column)",
    fixed = TRUE
  )
})
