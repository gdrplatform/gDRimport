# table with the exception data
#nolint start
EXCEPTION_TABLE <- tibble::tribble(
  ~status_code, ~title, ~sprintf_text, ~type, ~input_type,
  "1", "Error loading manifest", "There were errors loading manifest. See logs:\n%s", "error", "manifest",
  "2", "Manifest not found", "Please load manifest!", "error", "manifest",
  "3", "Error loading template", "Some of the template files entered in the manifest have not been uploaded:\n%s\nPlease check the uploaded files and their name (including extension)", "error", "template",
  "4", "Warning loading template", "Some uploaded files (**%s**) are not used", "warning", "template",
  "5", "Error loading template", "There were errors when loading the templates. See logs:\n%s", "error", "template",
  "6", sprintf("Missing data for some %s", stringi::stri_flatten(gDRutils::get_env_identifiers("barcode"), collapse = ", ")), "Some plates (**%s**) found in the manifest are not reported in the data files and will be discarded", "warning", "manifest",
  "7", sprintf("Missing data for some %s", stringi::stri_flatten(gDRutils::get_env_identifiers("barcode"), collapse = ", ")), "Some plates (**%s**) found in the data files are not reported in the manifest and will be discarded", "warning", "manifest",
  "8", "Error loading raw data", "Txt file in unsupported format. Please check if file uses tab(\t) as separator", "error", "raw data",
  "9", "Error loading raw data", "There were errors when loading raw data. See logs:\n%s", "error", "raw data",
  "10", "Error during merging data", "There were errors when merging data. See logs:\n%s", "error", "merging data",
  "11", "Missing template files", "Some template files are missing: %s", "error", "template",
  "12", "Error reading Manifest", "There were errors reading the manifest file. Please see the logs:\n%s", "error", "manifest",
  "13", "Manifest file format", "%s file format is not supported. Please convert your file to one of the following: %s", "error", "manifest",
  "14", "Manifest barcodes", "Barcodes in Manifest must be unique!", "error", "manifest",
  "15", "Template controls", "No untreated controls were found in the template. Please upload the appropriate template.", "error", "template",
  "16", "Instrument type", "Unrecognized instrument type", "error", "raw data",
  "17", "Template sheet name", "In untreated template file %s, sheet name must be %s", "error", "template",
  "18", "Template entry name", "In untreated template file %s, entries must be %s", "error", "template",
  "19", "Error loading template", "There were errors loading the template. Plase see the logs:\n%s", "error", "template",
  "20", "Error loading specific template", "There were errors loading template %s. Please see logs: \n%s", "error", "template",
  "21", "Error reading specific raw data", "Error reading %s", "error", "raw data",
  "22", "Error reading sheet in raw data file", "Error reading %s, sheet %s", "error", "raw data",
  "23", "Error with readout values in plates", "In result file %s (sheet %s) readout values are misplaced for plate %s", "error", "raw data",
  "24", "Error with template headers", "Template does not contain all expected headers for a '%s'. '%s' is/are required. Please correct your template or change the identifiers.", "error", "template",
  "25", "Error with template file sheets", "Template file %s does not contain %s sheets. Please correct your template.", "error", "template",
  "26", "Metadata special characters", "Metadata field names for %s cannot contain special characters or start with a number: %s", "error", "metadata",
  "27", "Metadata invalid", "Metadata field name: %s in %s is not valid (reserved for output)", "error", "metadata",
  "28", "Raw Data delimiter", "Can't guess separator fo the delimited files: %s", "error", "raw data",
  "29", "Raw Data EnVision", "Error reading %s: not an original EnVision .csv file", "error", "raw data",
  "30", "Raw Data Plate size", "Error reading %s: wrong plate size", "error", "raw data",
  "31", "Raw Data structure", "In result file %s (sheet %s) readout values are misplaced for plate %s.", "error", "raw data",
  "32", "Template File Gnumber and Concentratoin", "Template file(s) %s do/does not contain the same number of Gnumber_* and Concentration_* sheets. Gnumber_* and Concentration_* sheets are required. Please correct your template.", "error", "template"
)
#nolint end

#' get exception data
#'
#' @param status_code A numeric value
#'
#' @return A tibble row with exception data or all exceptions
#' @export
get_exception_data <- function(status_code = NULL) {
  if (!is.null(status_code)) {
    checkmate::assert_number(status_code)
    checkmate::assert_choice(toString(status_code), EXCEPTION_TABLE$status_code)

    EXCEPTION_TABLE[EXCEPTION_TABLE$status_code == status_code, ]
  } else {
    EXCEPTION_TABLE
  }
}
