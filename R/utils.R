#' standardize_record_values
#'
#' map values to a dictionary
#'
#' @param x a named array
#' @param dictionary a named array
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

#' standardize_df
#'
#' Transform all the columns in a data.table to character type, and transform 
#' data.frame to data.table
#'
#' @param df data.table or data.frame for standardization
#' 
#' @examples
#' standardize_df(iris)
#'
#' @return a standardized data.table
#' 
#' @export
#'
standardize_df <- function(df) {
  # Assertions:
  checkmate::assert_data_frame(df)
  dt <- data.table::setDT(df)
  dt[, names(dt) := lapply(.SD, as.character)]
}

#' read_ref_data
#'
#' Read reference data
#'
#' @param inDir a directory path of reference data
#' @param prefix a prefix of reference file names ('ref' by default)
#'
#' @return a list of reference data
#'
read_ref_data <- function(inDir, prefix = "ref") {
  # Assertions:
  checkmate::assert_string(inDir)
  checkmate::assert_string(prefix)

  files <- list.files(inDir, paste0(prefix, "_.+\\.tsv$"), full.names = TRUE)
  lFiles <- lapply(files, function(x) {
    read.table(x, sep = "\t", header = TRUE)
    })
  names(lFiles) <- gsub("\\.tsv", "", gsub(paste0("^", prefix, "_"), "", basename(files)))
  refKeys <- yaml::read_yaml(file.path(inDir, paste0(prefix, "_keys.yaml")))
  refRowMaps <- yaml::read_yaml(file.path(inDir, paste0(prefix, "_row_maps.yaml")))
  lFiles$ref_keys <- refKeys
  lFiles$ref_row_maps <- refRowMaps
  lFiles
}

#' Detect format of results data
#'
#' @param results_file path to results data
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
  results_data <- rio::import(results_file)
  
  if (all(c("System", "User", "Plate", "Plate-ID (Stacker)") %in% results_data[, 1])) {
    "Tecan"
  } else if (c("Repeat Barcode") %in% do.call(paste, results_data[, 2:3])) {
    "EnVision"
  } else {
    "long_tsv"
  }
}
