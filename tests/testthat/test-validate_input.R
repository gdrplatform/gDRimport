test_that("validate_gdr_input passes the clean bench test data", {
  threshold <- futile.logger::flog.threshold()
  idfs <- gDRutils::get_env_identifiers()
  inputs <- data1_inputs()
  md5 <- tools::md5sum(inputs)

  res <- validate_gdr_input(inputs)
  expect_s3_class(res, "gdr_validation")
  expect_identical(res$verdict, "PASS")
  expect_identical(res$data_type, "bench")
  expect_identical(sort(res$files$role), c("manifest", "raw", "raw", "template", "template"))
  expect_no_warn_fail(res)
  expect_finding(res, "load_data", "PASS")
  # Concentration_2 of the treated template is {0, 0.15}
  expect_finding(res, "combo_degenerate", "INFO")
  # every Duration + cell line group has 1 plate
  expect_finding(res, "plate_outliers", "INFO")
  expect_named(res$findings, c("check", "severity", "detail", "fix_tier", "status_code"))

  # the inputs, the logger threshold and the identifiers are left as they were
  expect_identical(tools::md5sum(inputs), md5)
  expect_identical(futile.logger::flog.threshold(), threshold)
  expect_identical(gDRutils::get_env_identifiers(), idfs)
})

test_that("validate_gdr_input passes the Tecan, EnVision csv and tsv test data", {
  expect_no_warn_fail(validate_gdr_input(data2_inputs()))

  # data4: an untreated template that only loads next to the treated one, lowercase
  # sheet names and 28 original EnVision csv files
  res4 <- validate_gdr_input(data4_inputs())
  expect_identical(as.vector(table(res4$files$role)[c("manifest", "raw", "template")]), c(1L, 28L, 2L))
  # row O of every plate has annotated wells without a readout
  expect_no_warn_fail(res4, allowed = "readout_values")

  # data5: medians over all wells fall from 0 h to 72 h, those of the control wells do not
  res5 <- validate_gdr_input(data5_inputs())
  expect_no_warn_fail(res5)
  expect_no_finding(res5, "duration_plausibility")
  # the untreated template has a Concentration_2 column with zeros only
  expect_no_finding(res5, "combo_degenerate")
})

test_that("validate_gdr_input accepts a directory and prints a summary", {
  dir <- dirname(copy_inputs(data5_inputs())[1])
  file.create(file.path(dir, ".DS_Store"))
  res <- validate_gdr_input(dir)
  expect_identical(NROW(res$files), 4L)
  expect_output(print(res), "VERDICT: PASS")
  expect_error(validate_gdr_input(file.path(dir, "missing.xlsx")), "not found")
})

test_that("validate_gdr_input reports missing and unusable files", {
  inputs <- data1_inputs()

  res <- validate_gdr_input(inputs[-1])
  expect_finding(res, "files", "FAIL", status_code = 2L)
  expect_no_finding(res, "load_data")
  expect_finding(validate_gdr_input(inputs[1:3]), "files", "FAIL", status_code = 43L)

  extra <- copy_inputs(data5_inputs())
  dir <- dirname(extra[1])
  file.create(file.path(dir, "empty.tsv"))
  writeLines("not a spreadsheet", file.path(dir, "plates.numbers"))
  res <- validate_gdr_input(dir)
  expect_finding(res, "empty_files", "FAIL", status_code = 41L)
  expect_finding(res, "file_format", "WARN", status_code = 13L)
  expect_finding(res, "files", "INFO")

  empty_dir <- tempfile()
  dir.create(empty_dir)
  expect_finding(validate_gdr_input(empty_dir), "type", "FAIL")
})

test_that("validate_gdr_input reports encoding and file name problems", {
  inputs <- copy_inputs(data5_inputs())
  manifest <- inputs[["Manifest.tsv"]]
  bytes <- readBin(manifest, "raw", file.size(manifest))
  # CL_B -> CL_<0xB5>B, a Windows-1252 "mu" sign
  at <- grepRaw("CL_B", bytes)
  writeBin(c(bytes[seq_len(at + 2L)], as.raw(0xB5), bytes[(at + 3L):length(bytes)]), manifest)
  expect_finding(validate_gdr_input(inputs), "encoding", "FAIL", status_code = 40L)

  inputs <- copy_inputs(data5_inputs())
  spaced <- file.path(dirname(inputs[1]), "Template_trt .tsv")
  file.rename(inputs[["Template_trt.tsv"]], spaced)
  res <- validate_gdr_input(c(inputs[-2], spaced))
  expect_finding(res, "filename_whitespace", "INFO")
  hit <- expect_finding(res, "templates", "WARN", status_code = 3L)
  expect_identical(hit$fix_tier, "A")
})

test_that("validate_gdr_input reports manifest problems", {
  inputs <- copy_inputs(data5_inputs())
  edit_tsv(inputs[["Manifest.tsv"]], function(dt) {
    dt$Template[1] <- "Template_trt"
    dt
  })
  res <- validate_gdr_input(inputs)
  hit <- expect_finding(res, "templates", "WARN", status_code = 3L)
  expect_identical(hit$fix_tier, "A")
  expect_finding(res, "load_data", "FAIL", status_code = 11L)

  inputs <- copy_inputs(data5_inputs())
  edit_tsv(inputs[["Manifest.tsv"]], function(dt) rbind(dt, dt[1]))
  expect_finding(validate_gdr_input(inputs), "manifest", "FAIL", status_code = 14L)

  inputs <- copy_inputs(data5_inputs())
  edit_tsv(inputs[["Manifest.tsv"]], function(dt) {
    dt$Duration[1] <- "three days"
    dt
  })
  expect_finding(validate_gdr_input(inputs), "duration", "WARN")
})

test_that("validate_gdr_input reports problems in xlsx templates", {
  treated <- "Template_7daytreated.xlsx"

  inputs <- copy_inputs(data1_inputs())
  wb <- openxlsx::loadWorkbook(inputs[[treated]])
  openxlsx::renameWorksheet(wb, "Gnumber", "Gnumber_1")
  openxlsx::renameWorksheet(wb, "Concentration", "Concentration_1")
  openxlsx::saveWorkbook(wb, inputs[[treated]], overwrite = TRUE)
  res <- validate_gdr_input(inputs)
  expect_finding(res, "sheet_naming", "WARN", status_code = 25L)
  expect_finding(res, "load_data", "FAIL")

  # column A is the empty plate border
  inputs <- copy_inputs(data1_inputs())
  set_xlsx_cell(inputs[[treated]], "Gnumber", 1, 1, "n/a")
  set_xlsx_cell(inputs[[treated]], "Concentration", 2, 1, "-99")
  res <- validate_gdr_input(inputs)
  expect_finding(res, "border_placeholder", "WARN")
  expect_no_finding(res, "concentration_not_numeric")

  inputs <- copy_inputs(data1_inputs())
  set_xlsx_cell(inputs[[treated]], "Concentration", 1, 3, "high")
  expect_finding(validate_gdr_input(inputs), "concentration_not_numeric", "FAIL", status_code = 44L)
})

test_that("validate_gdr_input checks identifier formats", {
  treated <- "Template_7daytreated.xlsx"
  inputs <- copy_inputs(data1_inputs())
  set_xlsx_cell(inputs[[treated]], "Gnumber", 1, 3, "G-03406036.1-1")
  res <- validate_gdr_input(inputs)
  expect_finding(res, "gnumber_format", "WARN")
  expect_no_finding(validate_gdr_input(inputs, id_patterns = list(drug = NULL)), "gnumber_format")

  inputs <- copy_inputs(data1_inputs())
  set_xlsx_cell(inputs[[treated]], "Gnumber", 1, 3, "G03406036.1-1")
  expect_no_finding(validate_gdr_input(inputs), "gnumber_format")

  inputs <- copy_inputs(data5_inputs())
  edit_tsv(inputs[["Manifest.tsv"]], function(dt) {
    dt$clid[dt$clid == "CL_B"] <- "CL 00011"
    dt
  })
  expect_finding(validate_gdr_input(inputs, id_patterns = list(cellline = "^CL[0-9]+")), "clid_format", "WARN")
  expect_no_finding(validate_gdr_input(inputs), "clid_format")

  inputs <- copy_inputs(data5_inputs())
  edit_tsv(inputs[["Manifest.tsv"]], function(dt) {
    dt$clid[dt$clid == "CL_B"] <- "CL_B "
    dt
  })
  expect_finding(validate_gdr_input(inputs), "clid_format", "WARN")
})

test_that("validate_gdr_input reports plates without a barcode", {
  # the barcode of plate 1 is in C3; the Background section below it has none
  inputs <- copy_inputs(data1_inputs())
  set_xlsx_cell(inputs[["RawData_day0.xlsx"]], 1, 3, 3, NA)
  hit <- expect_finding(validate_gdr_input(inputs), "plate_barcode", "FAIL", status_code = 45L)
  expect_match(hit$detail, "RawData_day0.xlsx plate 1")

  inputs <- copy_inputs(data1_inputs())
  barcodes <- data.table::as.data.table(which(
    openxlsx::read.xlsx(inputs[["RawData_day0.xlsx"]], colNames = FALSE, skipEmptyRows = FALSE,
                        skipEmptyCols = FALSE) == "Barcode",
    arr.ind = TRUE
  ))
  for (i in 1:2) set_xlsx_cell(inputs[["RawData_day0.xlsx"]], 1, barcodes$row[i] + 1L, barcodes$col[i], "N/A")
  res <- validate_gdr_input(inputs)
  expect_finding(res, "plate_barcode", "FAIL", status_code = 45L)
  expect_finding(res, "duplicate_wells", "FAIL", status_code = 46L)
})

test_that("validate_gdr_input reports problems in the loaded data", {
  inputs <- copy_inputs(data5_inputs())
  edit_tsv(inputs[["Manifest.tsv"]], function(dt) dt[-1])
  expect_finding(validate_gdr_input(inputs), "barcode_coverage", "WARN", status_code = 7L)

  inputs <- copy_inputs(data5_inputs())
  edit_tsv(inputs[["Manifest.tsv"]], function(dt) {
    extra <- dt[1]
    extra$Barcode <- "Plate99"
    rbind(dt, extra)
  })
  expect_finding(validate_gdr_input(inputs), "barcode_coverage", "WARN", status_code = 6L)

  inputs <- copy_inputs(data5_inputs())
  edit_tsv(inputs[["RawData.tsv"]], function(dt) {
    extra <- dt[1]
    extra$ReadoutValue <- "1"
    rbind(dt, extra)
  })
  expect_finding(validate_gdr_input(inputs), "duplicate_wells", "FAIL", status_code = 46L)

  inputs <- copy_inputs(data5_inputs())
  edit_tsv(inputs[["RawData.tsv"]], function(dt) {
    dt$ReadoutValue[1] <- "-5"
    dt$BackgroundValue[2] <- "1000"
    dt$ReadoutValue[dt$Barcode == "Plate2"] <- "50"
    dt[!(dt$Barcode == "Plate3" & dt$WellRow == "C")]
  })
  res <- validate_gdr_input(inputs)
  expect_finding(res, "negative_readouts", "FAIL", status_code = 47L)
  expect_finding(res, "background", "WARN")
  expect_finding(res, "flat_plates", "WARN")
  expect_finding(res, "well_count", "WARN")

  inputs <- copy_inputs(data5_inputs())
  edit_tsv(inputs[["RawData.tsv"]], function(dt) {
    dt$ReadoutValue[seq_len(300)] <- "0"
    dt
  })
  expect_finding(validate_gdr_input(inputs), "zero_readouts", "WARN")
})

test_that("validate_gdr_input finds plates that read far from comparable plates", {
  inputs <- copy_inputs(data5_inputs())
  edit_tsv(inputs[["Manifest.tsv"]], function(dt) {
    dt$clid <- "CL_A"
    dt$Duration <- "72"
    dt
  })
  res <- validate_gdr_input(inputs)
  expect_finding(res, "plate_outliers", "PASS")

  edit_tsv(inputs[["RawData.tsv"]], function(dt) {
    plate1 <- dt$Barcode == "Plate1"
    dt$ReadoutValue[plate1] <- as.character(as.numeric(dt$ReadoutValue[plate1]) * 20)
    dt
  })
  hit <- expect_finding(validate_gdr_input(inputs), "plate_outliers", "WARN")
  expect_match(hit$detail, "Plate1")
})

test_that("validate_gdr_input checks that Durations fit the readouts", {
  manifest <- "manifest.xlsx"

  # a day-0 plate labelled 168: that cell line has no day-0 plate
  inputs <- copy_inputs(data1_inputs())
  set_xlsx_cell(inputs[[manifest]], 1, 2, 2, 168)
  hit <- expect_finding(validate_gdr_input(inputs), "duration_plausibility", "WARN")
  expect_match(hit$detail, "CL00011")

  # Durations of the two CL00011 plates swapped: its control wells read lower at 168 h
  inputs <- copy_inputs(data1_inputs())
  set_xlsx_cell(inputs[[manifest]], 1, 2, 2, 168)
  set_xlsx_cell(inputs[[manifest]], 1, 3, 2, 0)
  hit <- expect_finding(validate_gdr_input(inputs), "duration_plausibility", "WARN")
  expect_match(hit$detail, "CL00011 \\(0h")
})

test_that("validate_gdr_input reports raw data from several instruments and long tables", {
  res <- validate_gdr_input(c(data1_inputs(), get_test_tsv_data()$r_files))
  expect_finding(res, "mixed_instruments", "WARN")

  inputs <- copy_inputs(data5_inputs())
  long <- file.path(dirname(inputs[1]), "Template_trt.xlsx")
  openxlsx::write.xlsx(data.table::fread(inputs[["Template_trt.tsv"]]), long)
  res <- validate_gdr_input(c(inputs[["Manifest.tsv"]], inputs[["Template_untrt.tsv"]], long, inputs[["RawData.tsv"]]))
  expect_finding(res, "long_format_treatment", "FAIL", status_code = 42L)
})

test_that("validate_gdr_input checks PRISM input", {
  testdata <- function(f) system.file("testdata", f, package = "gDRimport")

  res <- validate_gdr_input(c(testdata("prism_sa.csv"), testdata("prism_model.csv")))
  expect_identical(res$data_type, "prism")
  expect_identical(res$verdict, "PASS")
  expect_finding(res, "prism_level", "PASS")

  res <- validate_gdr_input(c(testdata("prism_collapsed_LOGFC.csv"), testdata("prism_cell_lines.csv"),
                              testdata("prism_treatment.csv"), testdata("prism_model.csv")))
  expect_identical(res$verdict, "PASS")
  expect_match(res$findings$detail[res$findings$check == "prism_level"], "LEVEL6")
  expect_finding(validate_gdr_input(c(testdata("prism_collapsed_LOGFC.csv"), testdata("prism_model.csv"))),
                 "prism_data", "FAIL")

  # the converter's synonyms count as the columns they stand for
  inputs <- copy_inputs(c(testdata("prism_sa.csv"), testdata("prism_model.csv")))
  dt <- data.table::fread(inputs[["prism_sa.csv"]])
  data.table::setnames(dt, c("LFC_cb", "LFC", "pert_iname"), c("l2fc", "old_lfc", "pert_name"))
  data.table::fwrite(dt, inputs[["prism_sa.csv"]])
  expect_identical(validate_gdr_input(inputs)$verdict, "PASS")

  meta <- data.table::fread(inputs[["prism_model.csv"]])
  meta$OncotreeLineage <- NULL
  data.table::fwrite(meta, inputs[["prism_model.csv"]])
  expect_finding(validate_gdr_input(inputs), "prism_meta", "FAIL")
})

test_that("validate_gdr_input checks MAE and D300 input", {
  bad_mae <- tempfile(fileext = ".qs2")
  writeLines("not a qs2 file", bad_mae)
  expect_finding(validate_gdr_input(bad_mae), "mae", "FAIL")

  # the package's own MAE still has the old "matrix" experiment
  legacy <- system.file("extdata", "kyung2022mae", "MAE_E2.qs2", package = "gDRimport")
  expect_match(expect_finding(validate_gdr_input(legacy), "mae", "FAIL")$detail, "matrix")

  skip_if_not_installed("gDRtestData")
  mae <- system.file("testdata", "finalMAE_small_no_noise.qs2", package = "gDRtestData")
  expect_finding(validate_gdr_input(mae), "mae", "PASS")

  res <- validate_gdr_input(get_test_D300_data()$f_96w$d300)
  expect_identical(res$data_type, "pre_d300")
  expect_finding(res, "stage", "INFO")
})
