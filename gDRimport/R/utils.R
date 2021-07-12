#' standardize_record_values
#'
#' map values to a dictionary
#'
#' @param x a named array
#' @param dictionary a named array
#'
#' @return a named array with updated names
#' @export
#'
standardize_record_values <- function(x, dictionary = DICTIONARY) {
  for (i in seq_along(dictionary)) {
    x[x == names(dictionary[i])] <- dictionary[[i]]
  }
  x
}

#' standardize_df
#'
#' Transform all the columns in a dataframe to character type
#'
#' @param df a dataframe for standardization
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
#' @export
#'
read_ref_data <- function(inDir, prefix = "ref") {
  # Assertions:
  checkmate::assert_string(inDir)
  checkmate::assert_string(prefix)

  files <- list.files(inDir, paste0(prefix, "_.+\\.tsv$"), full.names = TRUE)
  lFiles <- lapply(files, function(x) { read.table(x, sep = "\t", header = TRUE)})
  names(lFiles) <- gsub("\\.tsv", "", gsub(paste0("^", prefix, "_"), "", basename(files)))
  refKeys <- yaml::read_yaml(file.path(inDir, paste0(prefix, "_keys.yaml")))
  refRowMaps <- yaml::read_yaml(file.path(inDir, paste0(prefix, "_row_maps.yaml")))
  lFiles$ref_keys <- refKeys
  lFiles$ref_row_maps <- refRowMaps
  lFiles
}


#' write_ref_data_df
#'
#' Write reference dataframe
#'
#' @param lData a list with dataset
#' @param outDir an output directory
#' @param prefix a prefix of reference file names ('ref' by default)
#'
#' @return
#' @export
#'
write_ref_data_df <- function(lData, outDir, prefix = "ref") {
  # Assertions:
  checkmate::assert_list(lData)
  checkmate::assert_string(outDir)
  checkmate::assert_string(prefix)

  myL <- lapply(seq_along(lData), function(x) {
    outFile <- file.path(outDir, paste0(prefix, "_lData_", names(lData)[x], ".tsv"))
    write.table(lData[[x]], outFile, sep = "\t", quote = FALSE, row.names = FALSE)
  })

}


#' write_ref_data_se
#'
#' @param se a SummarizedExperiment with DR data
#' @param outDir an output directory
#' @param prefix a prefix of reference file names ('ref' by default)
#'
#' @return
#' @export
#'
write_ref_data_se <- function(se, outDir, prefix = "ref") {
  # Assertions:
  checkmate::assert_class(se, "SummarizedExperiment")
  checkmate::assert_string(outDir)
  checkmate::assert_string(prefix)
  #assays
  myL <- lapply(SummarizedExperiment::assayNames(se), function(x) {
    outFile <- file.path(outDir, paste0(prefix, "_assay_", x, ".tsv"))
    write.table(gDRutils::assay_to_dt(se, x, merge_metrics = TRUE), outFile, sep = "\t", quote = FALSE, row.names = FALSE)
  })

  #df_raw_data from metadata
  outFile <- file.path(outDir, paste0(prefix, "_df_raw_data.tsv"))
  write.table(metadata(se)$df_raw_data, outFile, sep = "\t", quote = FALSE, row.names = FALSE)


  keys_yaml <- yaml::as.yaml(metadata(se)$Keys)
  yaml::write_yaml(keys_yaml, file.path(outDir, paste0(prefix,"_keys.yaml")))

  row_maps_yaml <- yaml::as.yaml(metadata(se)$row_maps)
  yaml::write_yaml(row_maps_yaml, file.path(outDir, paste0(prefix,"_row_maps.yaml")))
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
#' @return
#' @export
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
