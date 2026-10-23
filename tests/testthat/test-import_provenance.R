####
# Provenance of the input data: load_data(), the PRISM converters, load_long_table()
####

test_that("load_data records the files, the reader and the corrections", {
  td <- get_test_data()
  l_data <- suppressMessages(load_data(manifest_path(td), template_path(td), result_path(td)))
  record <- get_import_provenance(l_data$data)

  expect_identical(record$gDRimport, as.character(utils::packageVersion("gDRimport")))
  expect_identical(record$instrument, "EnVision")
  files <- record$files
  expect_identical(files$role, c("manifest", rep("template", length(template_path(td))),
                                 rep("results", length(result_path(td)))))
  expect_identical(files$name, basename(c(manifest_path(td), template_path(td), result_path(td))))
  expect_identical(files$md5, unname(tools::md5sum(c(manifest_path(td), template_path(td),
                                                     result_path(td)))))
  # the example templates write the untreated tag in another case
  expect_gt(record$corrections$untreated_tags_standardized, 0)
  # plain data only, so the record can be stored in the provenance of an SE
  expect_true(gDRutils:::.is_plain_data(record))
})


test_that("the import log collects corrections only while it is open", {
  untreated <- gDRutils::get_env_identifiers("untreated_tag")[[1]]
  df <- data.table::data.table(a = c(toupper(untreated), "x", untreated))
  logged <- .with_import_log(.standardize_untreated_values(df))
  expect_identical(logged$value$a, c(untreated, "x", untreated))
  expect_identical(.summarize_import_events(logged$events), list(untreated_tags_standardized = 1))

  # outside a load the corrections still happen and nothing is logged
  expect_identical(.standardize_untreated_values(df)$a, c(untreated, "x", untreated))
  expect_null(.import_log_env$current)
})


test_that("renamed template sheets are logged", {
  tfiles <- list("prov_template1.xlsx" = c("Gnomber", "Concentration"),
                 "prov_template2.xlsx" = c("Gnumber", "Concentration"))
  for (nm in names(tfiles)) {
    sheets <- lapply(tfiles[[nm]], function(y) data.table::data.table(x = stats::runif(12)))
    names(sheets) <- tfiles[[nm]]
    openxlsx::write.xlsx(sheets, file.path(tempdir(), nm))
  }
  logged <- .with_import_log(correct_template_sheets(file.path(tempdir(), names(tfiles))))
  expect_identical(
    .summarize_import_events(logged$events)$template_sheets_renamed,
    "prov_template1.xlsx: Gnomber -> Gnumber"
  )
})


test_that("cells filled with 0 are counted", {
  df <- read_excel_to_dt(system.file("extdata/data1/RawData_day7.xlsx", package = "gDRimport"),
                         col_names = FALSE)
  df <- df[, !apply(df[seq_len(35), ], 2, function(x) all(is.na(x))), with = FALSE]
  bckd <- which(df[[1]] %in% "Background information")
  if (length(bckd) > 0) {
    df[bckd + 1, 1] <- df[bckd, 1]
    df[bckd, 1] <- ""
  }
  size <- .get_plate_size(df)
  plate_row <- which(df[[1]] %in% "Plate information")
  data_rows <- unlist(lapply(plate_row, function(x) (x + 4):(x + 4 + size[1] - 1)))

  # a complete plate is left alone
  logged <- .with_import_log(.fill_empty_wells(df, plate_row, data_rows, size[1], size[2]))
  expect_null(.summarize_import_events(logged$events))

  # once a row is empty, every empty cell of the plate data rows becomes "0"
  df[9, ] <- NA
  n_empty <- sum(is.na(df[data_rows, ]))
  logged <- .with_import_log(.fill_empty_wells(df, plate_row, data_rows, size[1], size[2]))
  expect_identical(.summarize_import_events(logged$events)$cells_filled_with_zero, as.numeric(n_empty))
})


test_that("the PRISM converters record their source files", {
  prism_data <- system.file("testdata/prism_sa.csv", package = "gDRimport")
  prism_meta <- system.file("testdata/prism_model.csv", package = "gDRimport")
  record <- get_import_provenance(suppressWarnings(suppressMessages(
    convert_LEVEL5_prism_to_gDR_input(prism_data, prism_meta)
  )))
  expect_identical(record$source, list(format = "PRISM LEVEL5", readout_min = 1.03))
  expect_identical(record$files$role, c("prism_data", "meta_data"))
  expect_identical(record$files$md5, unname(tools::md5sum(c(prism_data, prism_meta))))

  record6 <- get_import_provenance(suppressWarnings(suppressMessages(convert_LEVEL6_prism_to_gDR_input(
    system.file("testdata/prism_collapsed_LOGFC.csv", package = "gDRimport"),
    system.file("testdata/prism_cell_lines.csv", package = "gDRimport"),
    system.file("testdata/prism_treatment.csv", package = "gDRimport"),
    prism_meta
  ))))
  expect_identical(record6$source$format, "PRISM LEVEL6")
  expect_identical(record6$files$role, c("prism_data", "cell_line_data", "treatment_data", "meta_data"))
})


test_that("load_long_table records its file", {
  path <- tempfile(fileext = ".csv")
  data.table::fwrite(data.table::data.table(
    Gnumber = c("G1", "G1"), clid = "CL1", Duration = 72, Concentration = c(0, 1),
    ReadoutValue = c(1000, 500)
  ), path)
  record <- get_import_provenance(load_long_table(path))
  expect_identical(record$instrument, "long_table")
  expect_identical(record$files$md5, unname(tools::md5sum(path)))
})


test_that("plates read without a barcode are recorded", {
  raw <- file.path(tempfile(), "RawData_day0.xlsx")
  dir.create(dirname(raw))
  file.copy(system.file("extdata/data1/RawData_day0.xlsx", package = "gDRimport"), raw)
  td <- get_test_data()
  l_data <- suppressMessages(load_data(manifest_path(td), template_path(td), result_path(td)))
  expect_null(get_import_provenance(l_data$data)$corrections$plates_without_barcode)

  # the barcode of plate 1 is in C3
  wb <- openxlsx::loadWorkbook(raw)
  openxlsx::deleteData(wb, 1, cols = 3, rows = 3, gridExpand = TRUE)
  openxlsx::saveWorkbook(wb, raw, overwrite = TRUE)
  # the manifest headers, as load_data() passes them
  headers <- suppressMessages(load_manifest(manifest_path(td)))$headers
  logged <- .with_import_log(suppressMessages(load_results(raw, headers = headers)))
  missing <- .summarize_import_events(logged$events)$plates_without_barcode
  expect_identical(missing$file, "RawData_day0.xlsx")
  expect_identical(missing$plate, "1")
  expect_true(is.na(missing$header))
  # the Background section has no barcode, so its first cell below the header is used
  expect_false(is.na(missing$used))
  skip_if_not(exists(".is_plain_data", envir = asNamespace("gDRutils")))
  expect_true(gDRutils:::.is_plain_data(list(corrections = list(plates_without_barcode = missing))))
})
