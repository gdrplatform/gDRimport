context("load_files")
  
  test_that("load_manifest", {

  # get test_data1
  td1 <- get_test_data1()
 
  # valid output returned for "manifest.xlsx" 
  m_df <- load_manifest(td1$m_file)
  expect_identical(td1$ref_m_df, m_df)
  
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

  # valid output returned for the two xlsx files
  res_tbl <- load_results(df_results_files = c(td1$r_files))
  ## check with reference
  ## reference obtained with: write.csv2(res_tbl,file = "ref_RawData_day0_day7_xlsx.csv",row.names = FALSE) # nolint
  ref_tbl <- read.csv2(td1$ref_r1_r2)
  expect_equal(res_tbl, ref_tbl)
  
  # valid output returned for data.frame input
  df_results <- data.frame(datapath = td1$r_files, name = basename(td1$r_files))
  res_df_tbl <- load_results(df_results)
  expect_equal(res_df_tbl, ref_tbl)
  
  # valid output is returned for a single xlsx file
  res_tbl2 <- load_results(df_results_files = c(td1$r_files[1]))
  ## check with reference
  ref_tbl2 <- read.csv2(td1$ref_r1)
  expect_equal(res_tbl2, ref_tbl2)
  
  # expected error(s) returned
  err_msg1 <- "Assertion on 'results_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_results(c(td1$r_files[1], "/non/existent_file")), err_msg1)
  
  # expected error(s) returned
  err_msg2 <- "Assertion on 'instrument' failed: Must comply to pattern '^EnVision$|^long_tsv$'."
  expect_error(load_results(td1$r_files, "invalid_instrument"), err_msg2, fixed = TRUE)
})


test_that("load_templates", {
  
  # get test_data1
  td1 <- get_test_data1()

  # valid output returned for the two xlsx files
  t_tbl <- load_templates(df_template_files = c(td1$t_files))
  ## check with reference
  ## reference obtained with: write.csv2(t_tbl,file = "ref_template_treated_untreated_xlsx.csv",row.names = FALSE) # nolint
  ref_tbl <- read.csv2(td1$ref_t1_t2)
  expect_equal(standardize_df(t_tbl), standardize_df(ref_tbl))
  
  # valid output returned for data.frame input
  df_templates <- data.frame(datapath = td1$t_files, name = basename(td1$t_files))
  res_t_tbl <- load_templates(df_templates)
  expect_equal(standardize_df(res_t_tbl), standardize_df(ref_tbl))
  
  # valid output is returned for a single xlsx file
  t_tbl2 <- load_templates(df_template_files = c(td1$t_files[1]))
  ## check with reference
  ref_tbl2 <- read.csv2(td1$ref_t1)
  expect_equal(standardize_df(t_tbl2), standardize_df(ref_tbl2))
  
  # expected error(s) returned
  err_msg1 <- "Assertion on 'template_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_templates(c(td1$t_files[1], "/non/existent_file")), err_msg1)
})
  
test_that("load_data", {
  
  # get test_data1
  td1 <- get_test_data1()

  l_tbl <- load_data(td1$m_file, td1$t_files, td1$r_files) 
  # valid output returned for manifest
  expect_identical(td1$ref_m_df, l_tbl$manifest)
  # valid output returned for templates
  ref_tbl <- read.csv2(td1$ref_t1_t2)
  expect_equal(standardize_df(l_tbl$treatments), standardize_df(ref_tbl))
  # valid output returned for results 
  ref_tbl <- read.csv2(td1$ref_r1_r2)
  expect_equal(l_tbl$data, ref_tbl)
  
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
  err_msg5 <- "Assertion on 'instrument' failed: Must comply to pattern '^EnVision$|^long_tsv$'."
  expect_error(load_data(td1$m_file, td1$t_files, td1$r_files, "invalid_instrument"), err_msg5, fixed = TRUE)
  
  # expected error(s) returned - results
  err_msg6 <- "Assertion on 'results_file' failed: File does not exist: '/non/existent_file'."
  expect_error(load_data(td1$m_file, td1$t_files, c(td1$r_files[1], "/non/existent_file")), err_msg6)
  
})
