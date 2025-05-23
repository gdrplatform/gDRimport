# table with the exception data
#nolint start
EXCEPTION_TABLE <- tibble::tribble(
  ~status_code, ~title, ~sprintf_text, ~type, ~input_type,
  "1", "Error loading manifest", "There were errors loading manifest. Check error message below:\n```\n%s\n```", "error", "manifest",
  "2", "Manifest not found", "Please load manifest!", "error", "manifest",
  "3", "Error loading treatment", "Some of the treatment files entered in the manifest have not been uploaded:\n%s\nPlease check the uploaded files and their name (including extension)", "error", "treatment",
  "4", "Warning loading treatment", "Some uploaded files (**%s**) are not used", "warning", "treatment",
  "5", "Error loading treatment", "There were errors when loading the treatments. Check error message below:\n```\n%s\n```", "error", "treatment",
  "6", sprintf("Missing data for some %s", stringi::stri_flatten(gDRutils::get_env_identifiers("barcode"), collapse = ", ")), "Some plates (**%s**) found in the manifest are not reported in the data files and will be discarded", "warning", "manifest",
  "7", sprintf("Missing data for some %s", stringi::stri_flatten(gDRutils::get_env_identifiers("barcode"), collapse = ", ")), "Some plates (**%s**) found in the data files are not reported in the manifest and will be discarded", "warning", "manifest",
  "8", "Error loading raw data", "Txt file in an unsupported format. Please check if the file uses tab(\t) as separator", "error", "raw data",
  "9", "Error loading raw data", "There were errors when loading raw data. Check error message below:\n```\n%s\n```", "error", "raw data",
  "10", "Error during merging data", "There were errors when merging data. Check error message below:\n```\n%s\n```", "error", "merging data",
  "11", "Missing treatment files", "Some treatment files are missing: %s", "error", "treatment",
  "12", "Error reading Manifest", "There were errors reading the manifest file. Check error message below:\n```\n%s\n```", "error", "manifest",
  "13", "Manifest file format", "%s file format is not supported. Please convert your file to one of the following: %s", "error", "manifest",
  "14", "Manifest barcodes", "Barcodes in Manifest must be unique!", "error", "manifest",
  "15", "Treatment controls", "No untreated controls were found in the treatment. Please upload the appropriate treatment.", "error", "treatment",
  "16", "Instrument type", "Unrecognized instrument type", "error", "raw data",
  "17", "Treatment sheet name", "In untreated treatment file %s, sheet name must be %s", "error", "treatment",
  "18", "Treatment entry name", "In untreated treatment file %s, entries must be %s", "error", "treatment",
  "19", "Error loading treatment", "There were errors loading the treatment. Check error message below:\n```\n%s\n```", "error", "treatment",
  "20", "Error loading specific treatment", "There were errors loading treatment %s. Check error message below: \n%s", "error", "treatment",
  "21", "Error reading specific raw data", "Error reading %s", "error", "raw data",
  "22", "Error reading sheet in the raw data file", "Error reading %s, sheet %s", "error", "raw data",
  "23", "Error with readout values in plates", "In result file %s (sheet %s) readout values are misplaced for plate %s", "error", "raw data",
  "24", "Error with treatment headers", "Treatment does not contain all expected headers for a '%s'. '%s' is/are required. Please correct your treatment or change the identifiers.", "error", "treatment",
  "25", "Error with treatment file sheets", "Treatment file %s does not contain %s sheets. Please correct your treatment.", "error", "treatment",
  "26", "Metadata special characters", "Metadata field names for %s cannot contain special characters or start with a number: %s", "error", "metadata",
  "27", "Metadata invalid", "Metadata field name: %s in %s is not valid (reserved for output)", "error", "metadata",
  "28", "Raw Data delimiter", "Can't guess separator for the delimited files: %s", "error", "raw data",
  "29", "Raw Data EnVision", "Error reading %s: not an original EnVision .csv file", "error", "raw data",
  "30", "Raw Data Plate size", "Error reading %s: wrong plate size", "error", "raw data",
  "31", "Raw Data structure", "In result file %s (sheet %s) readout values are misplaced for plate %s.", "error", "raw data",
  "32", "Treatment File Gnumber and Concentration", "Treatment file(s) %s do/does not contain the same number of Gnumber_* and Concentration_* sheets. Gnumber_* and Concentration_* sheets are required. Please correct your treatment.", "error", "treatment",
  "33", "Error loading input files", "Some input files are missing. Please upload only files with names starting with Manifest_*, Treatment_* and RawData_*", "error", "input files",
  "34", "Missing drug annotation", "The following drugs IDs are missing: '%s'. <br>Don't worry, drug names will be used for them.", "warning", "submit tab",
  "35", "Missing cell line annotation", "The following cell line IDs are missing: '%s'. <br>Don't worry, cell line names will be used for them.", "warning", "submit tab",
  "36", "Invalid averaged data", "Averaged dose-response data for the selected cell line and drug: '%s' can be considered invalid. Please check your data in the module 'Manage Data' or contact gdrplatform team via 'gdr-support-d@gene.com'.", "error", "assay data"
)
#nolint end

#' get exception data
#'
#' @param status_code A numeric value
#'
#' @examples
#' get_exception_data(1)
#' get_exception_data()
#' @keywords correction_exception
#' 
#' @return A data.table row with exception data or all exceptions
#' @export
get_exception_data <- function(status_code = NULL) {
  checkmate::assert_number(status_code, null.ok = TRUE)
  
  res <- if (!is.null(status_code)) {
    checkmate::assert_choice(toString(status_code), EXCEPTION_TABLE$status_code)

    EXCEPTION_TABLE[EXCEPTION_TABLE$status_code == status_code, ]
  } else {
    EXCEPTION_TABLE
  }
  
  data.table::as.data.table(res)
}
