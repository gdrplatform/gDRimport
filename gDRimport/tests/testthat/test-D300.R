context("import_doses")

test_that("parse_D300_xml", {
  
  # get test_data3
  td3 <- get_test_data3()
  
  # valid output returned for the D300 96 well plate example
  dose_df <- parse_D300_xml(td3$d300_96w_file)
  ref_dose_df <- readRDS(td3$ref_d300_96w_file)
  expect_identical(dose_df, ref_dose_df)
  
  # valid output returned for the D300 348 well plate example
  dose_df <- parse_D300_xml(td3$d300_384w_file)
  ref_dose_df <- readRDS(td3$ref_d300_384w_file)
  expect_identical(dose_df, ref_dose_df)
  
  # expected error(s) returned
  err_msg1 <- "'D300_file' must be a readable path"
  expect_error(parse_D300_xml("/non/existent_file"), err_msg1)
  
})

test_that("import_D300", {
  
  # get test_data3
  td3 <- get_test_data3()
  
  # set directories for output and reference
  dest_paths <- list(td3$dest_path_d300_96w,  
                     td3$dest_path_d300_384w)
  ref_paths <- list(td3$ref_output_path_d300_96w,  
                     td3$ref_output_path_d300_384w)
  d300_files <- list(td3$d300_96w_file,  
                     td3$d300_384w_file)
  Gnum_files <- list(td3$Gnum_96w_file,  
                     td3$Gnum_384w_file)  
  
  #for 96 and 384 well plates
  for (k in seq_len(length(dest_paths))) {
    
    # validate output files for D300 examples
    dest_path <- dest_paths[[k]]
    ref_path <- ref_paths[[k]]
    D300_file <- d300_files[[k]]
    Gnum_file <- Gnum_files[[k]]
    
    # create directory if not existing
    if (!file.exists(dest_path)) {
      dir.create(dest_path, recursive = TRUE)
    } 
    # clean files from output directory 
    unlink(file.path(dest_path, "*"))
    # run import_D300
    import_D300(D300_file, Gnum_file, dest_path)
    
    # test every output file against reference file 
    list_files <- list.files(path = dest_path)
    for (i in seq_len(length(list_files))) {
      output_file_path <- file.path(dest_path, list_files[i])
      ref_file_path <- file.path(ref_path, list_files[i])
      #load sheets
      output_sheets <- readxl::excel_sheets(output_file_path)
      ref_sheets <- readxl::excel_sheets(ref_file_path) 
      #test sheet names are identical 
      expect_equal(output_sheets, ref_sheets)
      #test content of sheets is identical
      for (j in seq_len(length(output_sheets))) {
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


test_that("get_conversion_factor  works as expected", {
  expect_error(gDRimport:::get_conversion_factor("nL", "mL"), regexp = "conversion to unit 'mL' not supported")
  expect_error(gDRimport:::get_conversion_factor("L", "µL"), regexp = "unsupported conversion factor: 'L'")

  expect_equal(gDRimport:::get_conversion_factor("nL", "µL"), 1e-3)
})

test_that("convert_units works as expected", {
  expect_equal(gDRimport:::convert_units(1000, from = "mL", to = "µL"), 1000000)
})

#######
# Gnum
#######

test_that("gDRimport:::parse_D300_metadata_file works as expected", {
  
  # get test_data3
  td3 <- get_test_data3()
  
  # test Gnum files
  Gnum_96w_file <- gDRimport:::parse_D300_metadata_file(td3$Gnum_96w_file) 
  ref_Gnum_96w_file <- readRDS(td3$ref_Gnum_96w_file)
  expect_equal(Gnum_96w_file, ref_Gnum_96w_file)
  
  Gnum_384w_file <- gDRimport:::parse_D300_metadata_file(td3$Gnum_384w_file) 
  ref_Gnum_384w_file <- readRDS(td3$ref_Gnum_384w_file)
  expect_equal(Gnum_384w_file, ref_Gnum_384w_file)
})


#######
# Utils
#######

test_that("gDRimport:::fill_NA works as expected", {
  n <- 5
  df <- data.frame(a = rep(NA, n), b = seq(n))
  obs <- gDRimport:::fill_NA(df, "a", "b")
  expect_equal(obs$a, df$b)

  obs2 <- gDRimport:::fill_NA(df, "b", "a")
  expect_equal(obs2$b, df$b)
  expect_equal(obs2$a, df$a)
})
