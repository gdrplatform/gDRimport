test_that("fix = TRUE needs a new or empty output directory", {
  inputs <- data5_inputs()
  expect_error(validate_gdr_input(inputs, fix = TRUE), "output_dir")

  used <- tempfile()
  dir.create(used)
  file.create(file.path(used, "keep.txt"))
  expect_error(validate_gdr_input(inputs, fix = TRUE, output_dir = used), "new or empty")
  expect_error(validate_gdr_input(inputs, fix = TRUE, output_dir = file.path(used, "keep.txt")), "is a file")

  # two inputs with the same name cannot share the output directory
  twin <- copy_inputs(inputs[1])
  expect_error(validate_gdr_input(c(inputs, twin), fix = TRUE, output_dir = tempfile()), "share a name")
})

test_that("fix = TRUE copies clean data unchanged", {
  inputs <- data1_inputs()
  md5 <- tools::md5sum(inputs)
  res <- validate_gdr_input(inputs, fix = TRUE, output_dir = tempfile())

  expect_identical(res$verdict, "PASS")
  expect_identical(res$fixes, character(0))
  expect_true(dir.exists(res$output_dir))
  expect_identical(tools::md5sum(inputs), md5)
  expect_identical(unname(tools::md5sum(file.path(res$output_dir, basename(inputs)))), unname(md5))
  expect_identical(validate_gdr_input(inputs)$output_dir, NA_character_)
})

test_that("fix = TRUE repairs encoding, file names, placeholders and references in tsv input", {
  inputs <- copy_inputs(data5_inputs())
  dir <- dirname(inputs[1])
  manifest <- inputs[["Manifest.tsv"]]
  edit_tsv(manifest, function(dt) {
    dt$Template[dt$Template == "Template_untrt.tsv"] <- "Template_untrt"
    dt
  })
  # every CL_B -> CL_<0xB5>B, a Windows-1252 "mu" sign
  bytes <- readBin(manifest, "raw", file.size(manifest))
  text <- gsub("CL_B", "CL_\xb5B", rawToChar(bytes), fixed = TRUE, useBytes = TRUE)
  writeBin(charToRaw(text), manifest)
  file.rename(inputs[["Template_trt.tsv"]], file.path(dir, "Template_trt .tsv"))
  edit_tsv(inputs[["Template_untrt.tsv"]], function(dt) {
    dt$Concentration_2[1:3] <- "n/a"
    dt
  })
  md5 <- tools::md5sum(list.files(dir, full.names = TRUE))
  expect_identical(validate_gdr_input(dir)$verdict, "FAIL")

  res <- validate_gdr_input(dir, fix = TRUE, output_dir = tempfile())
  expect_identical(res$verdict, "PASS")
  expect_length(res$fixes, 4L)
  expect_true(all(c("Manifest.tsv", "Template_trt.tsv", "Template_untrt.tsv", "RawData.tsv") %in% res$files$file))
  expect_match(res$fixes, "re-encoded", all = FALSE)
  expect_match(res$fixes, "renamed to 'Template_trt.tsv'", all = FALSE)
  expect_match(res$fixes, "3 'n/a' or '-99' cell\\(s\\) cleared", all = FALSE)
  expect_match(res$fixes, "'Template_untrt' changed to 'Template_untrt.tsv'", all = FALSE)
  fixed_manifest <- data.table::fread(file.path(res$output_dir, "Manifest.tsv"))
  expect_true("CL_\u00b5B" %in% fixed_manifest$clid)
  # the user's files are not touched
  expect_identical(tools::md5sum(list.files(dir, full.names = TRUE)), md5)
})

test_that("fix = TRUE repairs xlsx templates and manifests cell by cell", {
  treated <- "Template_7daytreated.xlsx"
  inputs <- copy_inputs(data1_inputs())
  set_xlsx_cell(inputs[[treated]], "Gnumber", 1, 1, "n/a")
  set_xlsx_cell(inputs[[treated]], "Concentration", 2, 1, "-99")
  wb <- openxlsx::loadWorkbook(inputs[[treated]])
  openxlsx::renameWorksheet(wb, "Gnumber", "Gnumber_1")
  openxlsx::renameWorksheet(wb, "Concentration", "Concentration_1")
  openxlsx::saveWorkbook(wb, inputs[[treated]], overwrite = TRUE)
  # the manifest names the untreated template without its extension
  set_xlsx_cell(inputs[["manifest.xlsx"]], 1, 2, 3, "Template_Untreated")
  expect_identical(validate_gdr_input(inputs)$verdict, "FAIL")

  res <- validate_gdr_input(inputs, fix = TRUE, output_dir = tempfile())
  expect_identical(res$verdict, "PASS")
  expect_match(res$fixes, "sheet Gnumber_1 renamed to Gnumber", all = FALSE)
  expect_match(res$fixes, "2 'n/a' or '-99' cell\\(s\\) cleared", all = FALSE)
  expect_match(res$fixes, "'Template_Untreated' changed to 'Template_Untreated.xlsx'", all = FALSE)

  # only the placeholder cells changed; every other cell keeps its place and value
  read_sheet <- function(path, sheet) {
    as.matrix(readxl::read_excel(path, sheet = sheet, range = "A1:Z20", col_names = FALSE,
                                 col_types = "text", .name_repair = "minimal"))
  }
  original <- data1_inputs()[basename(data1_inputs()) == treated]
  fixed <- file.path(res$output_dir, treated)
  expect_identical(read_sheet(fixed, "Gnumber"), read_sheet(original, "Gnumber"))
  expect_identical(read_sheet(fixed, "Concentration"), read_sheet(original, "Concentration"))
  # numbers are still stored as numbers
  conc <- readxl::read_excel(fixed, sheet = "Concentration", range = "C1:D4", col_names = FALSE,
                             .name_repair = "minimal")
  expect_true(all(vapply(conc, is.numeric, logical(1))))
})

test_that("fix = TRUE saves long tables as tsv", {
  inputs <- copy_inputs(data5_inputs())
  long <- file.path(dirname(inputs[1]), "Template_trt.xlsx")
  openxlsx::write.xlsx(data.table::fread(inputs[["Template_trt.tsv"]]), long)
  file.remove(inputs[["Template_trt.tsv"]])
  res_input <- c(inputs[["Manifest.tsv"]], inputs[["Template_untrt.tsv"]], long, inputs[["RawData.tsv"]])
  expect_finding(validate_gdr_input(res_input), "long_format_treatment", "FAIL")

  res <- validate_gdr_input(res_input, fix = TRUE, output_dir = tempfile())
  expect_identical(res$verdict, "PASS")
  expect_match(res$fixes, "Template_trt.xlsx: long table saved as Template_trt.tsv", all = FALSE)
  expect_false(file.exists(file.path(res$output_dir, "Template_trt.xlsx")))
  expect_true(file.exists(long))
})

test_that("fix = TRUE keeps every value of a long table it converts", {
  # a barcode column that is numeric for the first 1050 rows and text after them, and
  # leading zeros, both of which type guessing would lose
  n <- 1100L
  long <- data.frame(Barcode = c(rep(1001, 1050), rep("P2-A", 50)), WellRow = "A",
                     WellColumn = rep(seq_len(24), length.out = n), ReadoutValue = seq_len(n),
                     Code = "0012", stringsAsFactors = FALSE)
  path <- file.path(tempfile(), "raw_long.xlsx")
  dir.create(dirname(path))
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "raw")
  openxlsx::writeData(wb, "raw", long[, -1])
  openxlsx::writeData(wb, "raw", "Barcode", startCol = 6, startRow = 1)
  openxlsx::writeData(wb, "raw", rep(1001, 1050), startCol = 6, startRow = 2)
  openxlsx::writeData(wb, "raw", rep("P2-A", 50), startCol = 6, startRow = 1052)
  openxlsx::saveWorkbook(wb, path)

  out <- file.path(tempfile(), "fixed")
  copies <- data.table::data.table(file = basename(path), path = path, role = "raw_long", format = NA_character_)
  .prepare_output_dir(out)
  file.copy(path, out)
  copies$path <- file.path(out, basename(path))
  step <- .fix_long_tables(copies)
  fixed <- data.table::fread(step$files$path, colClasses = "character")
  expect_identical(sum(fixed$Barcode == "P2-A"), 50L)
  expect_identical(unique(fixed$Code), "0012")
})
