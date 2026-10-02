#' standardize_record_values
#'
#' map values to a dictionary
#'
#' @param x a named array
#' @param dictionary a named array
#' @keywords utils
#'
#' @examples
#' standardize_record_values(c("Vehicle", "vehcle"))
#'
#' @return a named array with updated names
#'
#' @export
#'
standardize_record_values <- function(x, dictionary = DICTIONARY) {
  dnames <- names(dictionary)
  for (i in seq_along(dictionary)) {
    x[x == dnames[i]] <- dictionary[[i]]
  }
  x
}

#' read_ref_data
#'
#' Read reference data
#'
#' @param inDir a directory path of reference data
#' @param prefix a prefix of reference file names ('ref' by default)
#' @keywords utils
#'
#' @return a list of reference data
#'
#' @keywords internal
read_ref_data <- function(inDir, prefix = "ref") {
  # Assertions:
  checkmate::assert_string(inDir)
  checkmate::assert_string(prefix)

  files <- list.files(inDir, paste0(prefix, "_.+\\.tsv$"), full.names = TRUE)
  lFiles <- lapply(files, function(x) {
    data.table::fread(x, sep = "\t", header = TRUE)
    })
  names(lFiles) <- gsub("\\.tsv", "", gsub(paste0("^", prefix, "_"), "", basename(files)))
  refKeys <- yaml::read_yaml(file.path(inDir, paste0(prefix, "_keys.yaml")))
  refRowMaps <- yaml::read_yaml(file.path(inDir, paste0(prefix, "_row_maps.yaml")))
  lFiles$ref_keys <- refKeys
  lFiles$ref_row_maps <- refRowMaps
  lFiles
}

#' Assert that suggested packages required by a function are installed
#'
#' The PharmacoSet conversion functions are the only part of gDRimport that needs
#' CoreGx and PharmacoGx, and those two carry 37 further dependencies, so they are
#' suggested rather than imported. Call this before touching their API.
#'
#' @param pkgs character vector of package names.
#' @param purpose a short description of the functionality, used in the error message.
#' @keywords internal
#'
#' @return \code{NULL} invisibly.
#'
.assert_suggested_packages <- function(pkgs, purpose) {
  checkmate::assert_character(pkgs, min.len = 1)
  checkmate::assert_string(purpose)

  installed <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (!all(installed)) {
    missing_pkgs <- pkgs[!installed]
    stop(
      sprintf(
        "%s requires %s, which gDRimport suggests rather than imports. Install with BiocManager::install().",
        purpose,
        toString(missing_pkgs)
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Detect format of results data
#'
#' @param results_file path to results data
#' @keywords utils
#'
#' @examples
#' td2 <- get_test_Tecan_data()
#' detect_file_format(td2$r_files[1])
#'
#' @return string of the detected file format
#'
#' @export
detect_file_format <- function(results_file) {
  checkmate::assert_character(results_file)
  results_data <- rio::import(results_file, fill = TRUE)

  if (all(c("System", "User", "Plate", "Plate-ID (Stacker)") %in% results_data[, 1])) {
    "Tecan"
  } else if (any(grepl("Instrument Results from", c(names(results_data)[1],
                                                    results_data[, 1],
                                                    paste(results_data[1, ], collapse = " "))),
                 na.rm = TRUE)) {
    "EnVision_new"
  } else if ("Repeat Barcode" %in% c(
    do.call(paste, results_data[, 2:3]),
    paste(names(results_data[, 2:3]), collapse = " ")
  ) || any(grepl("Repeat,Barcode", results_data[, 1]))) {
    "EnVision"
  } else {
    "long_tsv"
  }
}

#' Read excel file and transorm it into data.table object
#'
#' @param path path to excel file
#' @param ... other arguments that should be passed into `readxl::read_excel`
#' @keywords utils
#'
#' @return data.table object with read excel file
#' @export
#'
#' @examples
#' datasets <- readxl::readxl_example("datasets.xlsx")
#' read_excel_to_dt(datasets)
read_excel_to_dt <- function(path, ...) {
  checkmate::assert_character(path)
  dt <- readxl::read_excel(path, ...)
  data.table::setDT(dt)
  dt
}
