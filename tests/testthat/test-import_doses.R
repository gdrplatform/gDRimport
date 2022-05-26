context("import_doses")

test_that("parse_D300", {
  
  # get test_data3
  td3 <- get_test_data3()
  
  # valid output returned for the D300 96 well plate example
  dose_df <- parse_D300(td3$d300_96w_file)
  ref_dose_df <- readRDS(td3$ref_d300_96w_file)
  expect_identical(dose_df, ref_dose_df)
  
  # valid output returned for the D300 348 well plate example
  dose_df <- parse_D300(td3$d300_384w_file)
  ref_dose_df <- readRDS(td3$ref_d300_384w_file)
  expect_identical(dose_df, ref_dose_df)
  
  # expected error(s) returned
  err_msg1 <- "'D300_file' must be a readable path"
  expect_error(parse_D300("/non/existent_file"), err_msg1)
  
})

test_that("import_D300", {
  
  # get test_data3
  td3 <- get_test_data3()
  
  # set directories for output and reference
  dest_paths <- list(td3$dest_path_d300_96w,  
                     td3$dest_path_d300_384w)
  ref_paths <- list(td3$ref_output_path_d300_96w,  
                     td3$ref_output_path_d300_384w)
  
  #for 96 and 384 well plates
  for (k in 1:length(dest_paths)) {
    
    # validate output files for D300 examples
    dest_path <- dest_paths[[k]]
    ref_path <- ref_paths[[k]]
    
    # create directory if not existing
    if (!file.exists(dest_path)) {
      dir.create(dest_path, recursive = TRUE)
    } 
    # clean files from output directory 
    unlink(file.path(dest_path, "*"))
    # run import_D300
    import_D300(td3$d300_96w_file, td3$Gnum_96w_file, dest_path)
    
    # test every output file against reference file 
    list_files <- list.files(path = dest_path)
    for (i in 1:length(list_files)) {
      output_file_path <- file.path(dest_path, list_files[i])
      ref_file_path <- file.path(ref_path, list_files[i])
      #load sheets
      output_sheets <- readxl::excel_sheets(output_file_path)
      ref_sheets <- readxl::excel_sheets(ref_file_path) 
      #test sheet names are identical 
      expect_equal(output_sheets, ref_sheets)
      #test content of sheets is identical
      for (j in 1:length(output_sheets)) {
          output_sheet <- readxl::read_excel(output_file_path,
                                             sheet = output_sheets[[j]],
                                             col_names = TRUE)
          ref_sheet <- readxl::read_excel(ref_file_path,
                                          sheet = ref_sheets[[j]],
                                          col_names = TRUE)
          expect_equal(output_sheet, ref_sheet)
      }    
    }
  }
})

