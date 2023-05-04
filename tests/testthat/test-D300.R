context("import_doses")
td3 <- get_test_D300_data()

test_that("parse_D300_xml", {
  
  # get test_D300_data
 
  # valid output returned for the D300 96 well plate example
  fs <- td3[["f_96w"]]
  dose_df <- parse_D300_xml(fs[["d300"]])
  ref_dose_df <- data.table::setDT(readRDS(fs[["ref_d300"]]))
  data.table::setorder(dose_df, Row, Col, D300_Plate_N)
  data.table::setorder(ref_dose_df, Row, Col, D300_Plate_N)
  expect_identical(dose_df, ref_dose_df)
  
  # valid output returned for the D300 348 well plate example
  fs2 <- td3[["f_384w"]]
  dose_df <- parse_D300_xml(fs[["d300"]])
  ref_dose_df <- data.table::setDT(readRDS(fs[["ref_d300"]]))
  data.table::setorder(dose_df, Row, Col, D300_Plate_N)
  data.table::setorder(ref_dose_df, Row, Col, D300_Plate_N)
  expect_identical(dose_df, ref_dose_df)
  
  # expected error(s) returned
  err_msg1 <- "'D300_file' must be a readable path"
  expect_error(parse_D300_xml("/non/existent_file"), err_msg1)
  
})

test_that("gDRimport:::parse_D300_metadata_file works as expected", {
  
  fs <- td3[["f_96w"]]
  Gnum_96w_file <- gDRimport:::parse_D300_metadata_file(fs$Gnum) 
  ref_Gnum_96w_file <- readRDS(fs$ref_Gnum)
  expect_equal(Gnum_96w_file, ref_Gnum_96w_file)
  
  fs <- td3[["f_384w"]]
  Gnum_96w_file <- gDRimport:::parse_D300_metadata_file(fs$Gnum) 
  ref_Gnum_96w_file <- readRDS(fs$ref_Gnum)
  expect_equal(Gnum_96w_file, ref_Gnum_96w_file)
})



test_that("import_D300", {
  
  on.exit({
    lapply(names(td3), function(x) {
      unlink(td3[[x]][["dest_path"]], recursive = TRUE)
    })
  })
  
  #for 96 and 384 well plates
  for (k in seq_along(td3)) {
    
    # validate output files for D300 examples
    fs <- td3[[k]]
    dest_path <- fs$dest_path
    ref_path <- fs$ref_output_path
    D300_file <- fs$d300
    Gnum_file <- fs$Gnum
   
    # create directory if not existing
    if (!file.exists(dest_path)) {
      dir.create(dest_path, recursive = TRUE)
    } 
    
    # run import_D300
    import_D300(D300_file, Gnum_file, dest_path)
    
    # test every output file against reference file 
    fs <- list.files(path = dest_path)
    idx <- c(1, length(fs)) # test with first and last file
    for (i in idx) {
      output_file_path <- file.path(dest_path, fs[i])
      ref_file_path <- file.path(ref_path, fs[i])
      #load sheets
      output_sheets <- readxl::excel_sheets(output_file_path)
      ref_sheets <- readxl::excel_sheets(ref_file_path) 
      #test sheet names are identical 
      expect_equal(output_sheets, ref_sheets)
      #test content of sheets is identical
      for (j in seq_len(length(output_sheets))) {
          output_sheet <- readxl::read_excel(output_file_path,
                                             sheet = output_sheets[[j]],
                                             col_names = FALSE)
          data.table::setDT(output_sheet)
          ref_sheet <- readxl::read_excel(ref_file_path,
                                          sheet = ref_sheets[[j]],
                                          col_names = FALSE)
          data.table::setDT(ref_sheet)
          untreated_tags <- gDRutils::get_env_identifiers("untreated_tag")
          ref_sheet[, names(ref_sheet) := lapply(.SD, function(x) {
            if (is.character(x)) {
              gsub(untreated_tags[[2]], untreated_tags[[1]], x)
            } else {
              x
            }
            }), .SDcols = names(ref_sheet)]
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
# Utils
#######

test_that("gDRimport:::fill_NA works as expected", {
  n <- 5
  df <- data.table::data.table(a = rep(NA, n), b = seq(n))
  obs <- gDRimport:::fill_NA(df, "a", "b")
  expect_equal(obs$a, df$b)

  obs2 <- gDRimport:::fill_NA(df, "b", "a")
  expect_equal(obs2$b, df$b)
  expect_equal(obs2$a, df$a)
})
