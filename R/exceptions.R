# table with the exception data
EXCEPTION_TABLE <- tibble::tribble(
  ~status_code, ~title, ~sprintf_text, ~type, ~input_type,
  "1", "Error loading manifest", "There were errors loading manifest. See logs:\n%s", "error", "manifest",
  "2", "Manifest not found", "Please load manifest!", "error", "manifest",
  "3", "Error loading template", "Some of the template files entered in the manifest have not been uploaded:\n%s\nPlease check the uploaded files and their name (including extension)", "error", "template",
  "4", "Error loading raw data", "Txt file in unsupported format. Please check if file uses tab(\t) as separator", "error", "raw data",
  "5", "Error loading raw data", "There were errors when loading raw data. See logs:\n%s", "error", "raw data",
  "6", "Error during merging data", "There were errors when merging data. See logs:\n%s", "error", "merging data",
  "7", "Missing template files", "Some template files are missing: %s", "error", "template",
  "8", "Error reading Manifest", "There were errors reading the manifest file. Please see the logs:\n%s", "error", "manifest",
  "9", "Manifest file format", "%s file format is not supported. Please convert your file to one of the following: %s", "error", "manifest",
  "10", "Manifest barcodes", "Barcodes in Manifest must be unique!", "error", "manifest",
  "11", "Template controls", "No untreated controls were found in the template. Please upload the appropriate template.", "error", "template",
  "12", "Instrument type", "Unrecognized instrument type", "error", "raw data",
  "13", "Template sheet name", "In untreated template file %s, sheet name must be %s", "error", "template",
  "14", "Template entry name", "In untreated template file %s, entries must be %s", "error", "template",
  "15", "Error loading template", "There were errors loading the template. Plase see the logs:\n%s", "error", "template",
  "16", "Error loading specific template", "There were errors loading template %s. Please see logs: \n%s", "error", "template",
  "17", "Error reading specific raw data", "Error reading %s", "error", "raw data",
  "18", "Error reading sheet in raw data file", "Error reading %s, sheet %s", "error", "raw data",
  "19", "Error with readout values in plates", "In result file %s (sheet %s) readout values are misplaced for plate %s", "error", "raw data",
  "20", "Error with template headers", "Template does not contain all expected headers for a '%s'. '%s' is/are required. Please correct your template or change the identifiers.", "error", "template",
  "21", "Error with template file sheets", "Template file %s does not contain %s sheets. Please correct your template.", "error", "template",
  "22", "Metadata special characters", "Metadata field names for %s cannot contain special characters or start with a number: %s", "error", "metadata",
  "23", "Metadata invalid", "Metadata field name: %s in %s is not valid (reserved for output)", "error", "metadata",
  "24", "Raw Data delimiter", "Can't guess separator fo the delimited files: %s", "error", "raw data",
  "25", "Raw Data EnVision", "Error reading %s: not an original EnVision .csv file", "error", "raw data",
  "26", "Raw Data Plate size", "Error reading %s: wrong plate size", "error", "raw data",
  "27", "Raw Data structure", "In result file %s (sheet %s) readout values are misplaced for plate %s.", "error", "raw data",
)

#' get exception data
#'
#' @param status_code A numeric value
#'
#' @return A tibble row with exception data or all exceptions
#' @export
get_exception_data <- function(status_code = NULL) {
  if (!is.null(status_code)) {
    EXCEPTION_TABLE[EXCEPTION_TABLE$status_code == status_code, ]
  } else {
    EXCEPTION_TABLE
  }
}
