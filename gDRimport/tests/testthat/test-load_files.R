context("load_files")

  test_that("load_manifest", {

  ## define ref data
  bcode_tbl <- expand.grid(c("201904190","201904197"), letters[1:6])
  bcode_v <- paste0(bcode_tbl$Var1, bcode_tbl$Var2)
  templates_v <- c("Template_Untreated.xlsx", "Template_7daytreated.xlsx")
  clids_n <- c(rep(11, 2), rep(12, 2), rep(13, 2), rep(14, 2), rep(15, 2), rep(18, 2))
  ref_df <- data.frame(Barcode = bcode_v, Duration = rep(c(0, 168), 6), Template = rep(templates_v, 6), clid = paste0("CL000",clids_n))
 
  # valid output returned for "manifest.xlsx" 
  m_file <- system.file(package = "gDRimport", "extdata", "data1", "manifest.xlsx")
  m_df <- load_manifest(m_file)
  expect_identical(ref_df, m_df)
  
  # expected error(s) returned
  err_msg1 <- "Assertion on 'manifest_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_manifest("/non/existent_file"),err_msg1)
  
  err_msg2 <- "'manifest_file' must be a character vector"
  expect_error(load_manifest(c(2,3)), err_msg2)
 
  err_msg3 <- "Barcodes in Manifest must be unique!" 
  expect_error(load_manifest(c(m_file, m_file)), err_msg3)
  
})

test_that("load_results", {

  # valid output returned for the two xlsx files
  r_files <-
    c(
      system.file(package = "gDRimport", "extdata", "data1", "RawData_day0.xlsx"),
      system.file(package = "gDRimport", "extdata", "data1", "RawData_day7.xlsx")
    )
  res_tbl <- gDRimport::load_results(df_results_files = c(r_files))
  ## check with reference
  ## reference obtained with: write.csv2(res_tbl,file = "ref_RawData_day0_day7_xlsx.csv",row.names = FALSE) # nolint
  ref_file <- system.file(package = "gDRimport", "extdata", "data1", "ref_RawData_day0_day7_xlsx.csv")
  ref_tbl <- read.csv2(ref_file)
  expect_equal(res_tbl, ref_tbl)
  
  # valid output returned for data.frame input
  df_results <- data.frame(datapath = r_files, name = basename(r_files))
  res_df_tbl <- gDRimport::load_results(df_results)
  expect_equal(res_df_tbl, ref_tbl)
  
  # valid output is returned for a single xlsx file
  res_tbl2 <- gDRimport::load_results(df_results_files = c(r_files[1]))
  ## check with reference
  ref_file2 <- system.file(package = "gDRimport", "extdata", "data1", "ref_RawData_day0_xlsx.csv")
  ref_tbl2 <- read.csv2(ref_file2)
  expect_equal(res_tbl2, ref_tbl2)
  
  # expected error(s) returned
  err_msg1 <- "Assertion on 'results_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_results(c(r_files[1], "/non/existent_file")),err_msg1)
  
  # expected error(s) returned
  err_msg2 <- "Assertion on 'instrument' failed: Must comply to pattern '^EnVision$|^long_tsv$'."
  expect_error(load_results(r_files, "invalid_instrument"), err_msg2, fixed = TRUE)
})

