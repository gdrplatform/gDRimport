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
#' Transform all the columns in a dataframe to character type
#'
#' @param df a dataframe for standardization
#' 
#' @examples
#' standardize_df(iris)
#'
#' @return a standardized dataframe
#' @export
#'
standardize_df <- function(df) {
  # Assertions:
  stopifnot(inherits(df, "data.frame"))
  data.frame(lapply(df, as.character))
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

#' save_file_type_info
#'
#' Save information about file types
#'
#' @param v a list with filenames
#' @param save_dir an output directory
#' @param normKeysFileName a filename with normalization keys ('normKeys.json' by default)
#' @param dfRawDataFileName a filename with rawdata ('dfRawData.tsv' by default)
#' @param fileTypeInfoName a filename with file type information ('fileTypeInfo.csv' by default)
#'
save_file_type_info <-
  function(v,
           save_dir,
           normKeysFileName = "normKeys.json",
           dfRawDataFileName = "dfRawData.tsv",
           fileTypeInfoName = "fileTypeInfo.csv") {
    # Assertions:
    checkmate::assert_list(v)
    checkmate::assert_string(normKeysFileName)
    checkmate::assert_string(dfRawDataFileName)
    checkmate::assert_string(fileTypeInfoName)
    checkmate::assert_true(all(c("Manifest", "Template", "RawData") %in% names(v)))

    tbl <- tibble::tibble(
      data_type = c(
        rep("manifest", length(v$Manifest$name)),
        rep("template", length(v$Template$name)),
        rep("rawData", length(v$RawData$name)),
        "normKeys",
        "dfRawData"
      ),
      name = c(
        v$Manifest$name,
        v$Template$name,
        v$RawData$name,
        normKeysFileName,
        dfRawDataFileName
      )
    )
    outFile <- file.path(save_dir, fileTypeInfoName)
    write.csv(tbl, outFile, row.names = FALSE)
  }



#' Detect format of results data
#'
#' @param results_file path to results data
#'
#' @examples 
#' td2 <- get_test_data2()
#' detect_file_format(td2$r_files[1])
#' 
#' @return string of the detected file format
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
