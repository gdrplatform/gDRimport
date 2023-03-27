#' Load data
#'
#' This functions loads and checks the data file(s)
#'
#' @param manifest_file character, file path(s) to manifest(s)
#' @param df_template_files data.frame, with datapaths and names of results file(s)
#' or character with file path of templates file(s)
#' @param results_file  data.frame, with datapaths and names of results file(s)
#' or character with file path of results file(s)
#' @param instrument character
#'
#' @export
#'
load_data <-
  function(manifest_file,
           df_template_files,
           results_file,
           instrument = "EnVision") {

    assertthat::assert_that(is.character(manifest_file), msg = "'manifest_file' must be a character vector")
    assertthat::assert_that(assertthat::is.readable(manifest_file), msg = "'manifest_file' must be a readable path")
    assertthat::assert_that(is.character(df_template_files) || is.data.frame(df_template_files),
                msg = "'df_template_files' must be a character vector or data.frame")
    assertthat::assert_that(is.character(results_file) || is.data.frame(results_file),
                msg = "'results_file' must be a character vector or data.frame")
    assertthat::assert_that(assertthat::is.string(instrument), msg = "'instrument' must be a character vector")

    if (is.data.frame(df_template_files)) {
      template_file <- df_template_files$datapath # for the shiny app
      is_readable_v(template_file)
      template_filename <- df_template_files$name
    } else {
      template_filename <- df_template_files
      is_readable_v(template_filename)
    }

    manifest_list <- load_manifest(manifest_file)
    manifest <- manifest_list$data
    headers <- manifest_list$headers
    treatments <- load_templates(df_template_files)
    data <- load_results(results_file, instrument, headers = headers)

      # check the all template files are available
    if (!all(unique(manifest[[headers[["template"]]]][manifest[[headers[["barcode"]]]] %in%
                                                      data[[headers[["barcode"]]]]])
             %in% basename(template_filename))) {
      exception_data <- get_exception_data(11)
      stop(sprintf(
        exception_data$sprintf_text,
        toString(setdiff(
          unique(manifest[[headers[["template"]]]][manifest[[headers[["barcode"]]]] %in%
                                                     data[[headers[["barcode"]]]]]),
          basename(template_filename)
        ))
      ))
    }
    return(list(
      manifest = manifest,
      treatments = treatments,
      data = data
    ))
  }


#' Load manifest
#'
#' This functions loads and checks the manifest file(s)
#'
#' @param manifest_file character, file path(s) to manifest(s)
#'
#' @export
load_manifest <- function(manifest_file) {
  # manifest_file is a string or a vector of strings
  available_formats <- c("text/tsv",
                         "text/tab-separated-values",
                         "xlsx",
                         "xls",
                         "tsv")
  assertthat::assert_that(is.character(manifest_file), msg = "'manifest_file' must be a character vector")
  checkmate::assert_file_exists(manifest_file)


  manifest_data <- read_in_manifest_file(manifest_file, available_formats)
  headers <- gDRutils::validate_identifiers(do.call(rbind, manifest_data), req_ids = "barcode")

  # check default headers are in each df
  dump <- lapply(seq_along(manifest_file),
                 function(i)
                   check_metadata_names(
                     colnames(manifest_data[[i]]),
                     df_name = manifest_file[[i]],
                     df_type = "manifest"
                   ))

  cat_manifest_data <- as.data.frame(data.table::rbindlist(manifest_data))
  colnames(cat_manifest_data) <-
    check_metadata_names(colnames(cat_manifest_data), "manifest")

  # check that barcodes are unique
  exception_data <- get_exception_data(14)
  if (dim(cat_manifest_data)[1] != length(unique(cat_manifest_data[[headers[["barcode"]]]])))
    stop(exception_data$sprintf_text)

  cat_manifest_data[[headers[["template"]]]] <- basename(cat_manifest_data[[headers[["template"]]]])

  futile.logger::flog.info("Manifest loaded successfully")
  return(list(data = cat_manifest_data,
              headers = headers))
}
  
#' read manifest files
#' 
#' @param manifest_file character, file path(s) to manifest(s)
#' @param available_formats charvec with available file formats
#' 
read_in_manifest_file <- function(manifest_file, available_formats) {
  
  manifest_data <- lapply(manifest_file, function(x) {
    manifest_ext <- tools::file_ext(x)
    if (manifest_ext %in% c("xlsx", "xls")) {
      df <- tryCatch({
        readxl::read_excel(x, col_names = TRUE)
      }, error = function(e) {
        exception_data <- get_exception_data(12)
        stop(sprintf(
          exception_data$sprintf_text,
          e
        ))
      })
    } else if (manifest_ext %in% c("text/tsv",
                                   "text/tab-separated-values",
                                   "tsv")) {
      df <- tryCatch({
        utils::read.table(x, sep = "\t", header = TRUE, na.strings = c("", "NA")) %>%
          stats::na.omit()
      }, error = function(e) {
        exception_data <- get_exception_data(12)
        stop(sprintf(
          exception_data$sprintf_text,
          e
        ))
      })
    } else {
      exception_data <- get_exception_data(13)
      stop(
        sprintf(
          exception_data$sprintf_text,
          manifest_ext,
          stringi::stri_flatten(available_formats, collapse = ", ")
        )
      )
    }
  })

  # replace synonyms, e.g. 'time => Duration' (backwards compatibility)
  manifest_data <- lapply(manifest_data, function(x) {
    colnames(x) <- gDRutils::update_idfs_synonyms(colnames(x))
    x
  })
  manifest_data
}

#' Load templates
#'
#' This functions loads and checks the template file(s)
#'
#' @param df_template_files data.frame, with datapaths and names of results file(s)
#' or character with file path of templates file(s)
#' @export
load_templates <- function(df_template_files) {
  # Assertions:
  stopifnot(any(inherits(df_template_files, "data.frame"), checkmate::test_character(df_template_files)))

  # template_file is a string or a vector of strings
  if (is.data.frame(df_template_files)) {
    # for the shiny app
    template_file <- df_template_files$datapath
    template_filename <- df_template_files$name
  } else {
    template_file <- df_template_files
    template_filename <- basename(template_file)
  }

  checkmate::assert_file_exists(template_file)

  all_templates <- data.frame()
  if (any(grepl("\\.xlsx?$", template_filename))) {
    idx <- grepl("\\.xlsx?$", template_filename)
    futile.logger::flog.info("Reading %s with load_templates_xlsx", template_filename[idx])
    all_templates_1 <-
      load_templates_xlsx(template_file[idx], template_filename[idx])
    all_templates <- rbind(all_templates, all_templates_1)
  }
  if (any(grepl("\\.[ct]sv$", template_filename))) {
    idx <- grepl("\\.[ct]sv$", template_filename)
    futile.logger::flog.info("Reading %s with load_templates_tsv", template_filename[idx])
    all_templates_2 <-
      load_templates_tsv(template_file[idx], template_filename[idx])
    all_templates <- rbind(all_templates, all_templates_2)
  }

  all_templates[[gDRutils::get_env_identifiers("drug")]] <-
    standardize_record_values(all_templates[[gDRutils::get_env_identifiers("drug")]], dictionary = DICTIONARY)

  if (all(!gDRutils::get_env_identifiers("untreated_tag") %in% unlist(all_templates))) {
    exception_data <- get_exception_data(15)
    stop(exception_data$sprintf_text)
  }

  return(all_templates)

}


#' Load results
#'
#' This functions loads and checks the results file(s)
#'
#' @param df_results_files  data.frame, with datapaths and names of results file(s)
#' or character with file path of results file(s)
#' @param instrument character
#' @param headers list of headers identified in the manifest file
#' @export
#'
load_results <-
  function(df_results_files, instrument = "EnVision", headers = gDRutils::get_env_identifiers()) {
    stopifnot(any(inherits(df_results_files, "data.frame"), checkmate::test_character(df_results_files)))
    checkmate::assert_string(instrument, pattern = "^EnVision$|^long_tsv$|^Tecan$")
    checkmate::assert_list(headers, null.ok = TRUE)
    if (is.data.frame(df_results_files)) {
      # for the shiny app
      results_file <- df_results_files$datapath
      results_filename <- df_results_files$name
    } else {
      results_file <- df_results_files
      results_filename <- basename(results_file)
    }
    checkmate::assert_file_exists(results_file)

    if (instrument == "EnVision") {
      all_results <-
        load_results_EnVision(results_file, headers = headers)
    } else if (instrument == "long_tsv") {
      all_results <-
        load_results_tsv(results_file, headers = headers)
    } else if (instrument == "Tecan") {
      all_results <-
        load_results_Tecan(results_file, headers = headers)
    } else {
      exception_data <- get_exception_data(16)
      stop(exception_data$sprintf_text)
    }
    return(unique(all_results))
  }


# individual functions

#' Load templates from tsv
#'
#' This functions loads and checks the template file(s)
#'
#' @param template_file character, file path(s) to template(s)
#' @param template_filename character, file name(s)
load_templates_tsv <-
  function(template_file,
           template_filename = NULL) {
    # Assertions:
    checkmate::assert_character(template_file)
    checkmate::assert_character(template_filename, null.ok = TRUE)


    if (is.null(template_filename))
      template_filename <- basename(template_file)

    # read columns in files
    templates <- lapply(template_file, function(x)
      utils::read.table(x, sep = "\t", header = TRUE, na.strings = c("", "NA")) %>%
        stats::na.omit())
    names(templates) <- template_filename
    # check WellRow/WellColumn is present in each df
    dump <- lapply(seq_along(template_file),
                   function(i)
                     if (!(all(
                       gDRutils::get_env_identifiers("well_position") %in% colnames(templates[[i]])
                     ))) {
                       futile.logger::flog.info("%s missing, %s as header",
                                                template_filename[[i]],
                                                gDRutils::get_env_identifiers("well_position"))
                     })
    # check drug_identifier is present in each df
    dump <- lapply(seq_along(template_file),
                   function(i)
                     check_metadata_names(
                       setdiff(colnames(templates[[i]]), gDRutils::get_env_identifiers("well_position")),
                       df_name = template_filename[[i]],
                       df_type = "template"
                     ))

    metadata_fields <- NULL
    all_templates <- read_in_tsv_template_files <- function(template_file, tmplate_filename, templates)
    futile.logger::flog.info("Templates loaded successfully!")
    all_templates
  }

#' read in tsv template files
#' 
#' @param template_file character, file path(s) to template(s)
#' @param template_filename character, file name(s)
#' @param templates list with templates data
#' 
read_in_tsv_template_files <- function(template_file, template_filename, templates) {
    tmpl_l <- lapply(template_file, function(iF) {
      futile.logger::flog.info("Loading %s", template_filename[iF])
      # 1) check that the sheet names are ok and 2) identify drug_identifier sheet (case insensitive)
      Gnumber_idx <- grep(paste0(gDRutils::get_env_identifiers("drug"), "$"),
                          colnames(templates[[iF]]), ignore.case = TRUE)
      Conc_idx <-
        grepl("Concentration", colnames(templates[[iF]]), ignore.case = TRUE)
      # case of untreated plate
      if (sum(Conc_idx) == 0) {
        if (length(Gnumber_idx) == 0) {
          exception_data <- get_exception_data(17)
          stop(sprintf(exception_data$sprintf_text, template_file[[iF]], gDRutils::get_env_identifiers("drug")))
        }
        df <- templates[[iF]][, gDRutils::get_env_identifiers("drug")]
        if (!(all(toupper(df)[!is.na(df)]) %in% toupper(gDRutils::get_env_identifiers("untreated_tag")))) {
          exception_data <- get_exception_data(18)
          stop(sprintf(exception_data$sprintf_text, template_file[[iF]],
            paste(gDRutils::get_env_identifiers("untreated_tag"), collapse = " or ")))
        }
      } else {
        # normal case
        check_metadata_names(colnames(templates[[iF]]),
                             df_name = template_filename[iF],
                             df_type = "template_treatment")
      }
      df_template <- templates[[iF]]
      for (iS in colnames(df_template)) {
        # check if metadata field already exist and correct capitalization if needed
        if (!(iS %in% metadata_fields)) {
          if (!is.null(metadata_fields) &&
              toupper(iS) %in% toupper(metadata_fields)) {
            oldiS <- iS
            iS <-
              metadata_fields[toupper(iS) == toupper(metadata_fields)]
            futile.logger::flog.info("%s corrected to match case with %s", oldiS, iS)
            colnames(df_template)[colnames(df_template) == oldiS] <-
              iS
          } else {
            metadata_fields <- c(metadata_fields, iS)
          }
        }
      }
      df_template$Template <- template_filename[iF]
      colnames(df_template) <-
        check_metadata_names(colnames(df_template), df_name = template_filename[iF])
    })
    do.call(rbind, tmpl_l)
}

#' Load templates from xlsx
#'
#' This functions loads and checks the template file(s)
#'
#' @param template_file character, file path(s) to template(s)
#' @param template_filename character, file name(s)
#'
load_templates_xlsx <-
  function(template_file,
           template_filename = NULL) {
    # Assertions:
    checkmate::assert_character(template_file)
    checkmate::assert_character(template_filename, null.ok = TRUE)

    if (is.null(template_filename))
      template_filename <- basename(template_file)

    # validate template sheets
    template_sheets <- correct_template_sheets(template_file)
    # check drug_identifier is present in each df
    dump <- lapply(seq_along(template_file),
                   function(i)
                     check_metadata_names(
                       template_sheets[[i]],
                       df_name = template_file[[i]],
                       df_type = "template"
                     ))

    all_templates <- read_in_template_xlsx(template_file, template_filename, template_sheets)
    
    # standardize untreated values
    all_templates <- .standardize_untreated_values(all_templates)
    futile.logger::flog.info("Templates loaded successfully!")
    return(all_templates)
  }

#' Read in xlsx template files
#' 
#' @param template_file character, file path(s) to template(s)
#' @param template_filename character, file name(s)
#' @param template_sheets template sheet names
read_in_template_xlsx <- function(template_file, template_filename, template_sheets) {
  
    metadata_fields <- NULL
    all_templates <- data.frame()
    for (iF in seq_along(template_file)) {
      futile.logger::flog.info("Loading %s", template_filename[iF])
      Gnumber_idx <- which(template_sheets[[iF]] %in% gDRutils::get_env_identifiers("drug"))
      
      # first check that the sheet names are ok
      # identify drug_identifier sheet
      validate_template_xlsx(template_file, template_filename, template_sheets, iF)
      
      # read the different sheets and check for plate size
      # enforce range to avoid skipping empty rows at the beginning
      plate_info <- get_plate_info_from_template_xlsx(template_file, Gnumber_idx, iF)
      df_template <-
        read_in_template_sheet_xlsx(template_file,
                                    template_sheets,
                                    iF,
                                    plate_info)
      df_template$Template <- template_filename[iF]
      colnames(df_template) <-
        check_metadata_names(colnames(df_template),
                             df_name = template_filename[iF])
      all_templates <- as.data.frame(data.table::rbindlist(list(all_templates, df_template), fill = TRUE))

    }
    all_templates
}

#' Read in data from xlsx template sheet
#' 
#' @param template_file character, file path(s) to template(s)
#' @param template_sheets template sheet names
#' @param idx template file index
#' @param plate_info list with plate info
#' 
read_in_template_sheet_xlsx <- function(template_file, template_sheets, idx, plate_info) {
  metadata_fields <- NULL
  # need to adapt for 1536 well plates
  df_template <-
    base::expand.grid(WellRow = LETTERS[seq_len(plate_info$n_row)],
                      WellColumn = seq_len(plate_info$n_col))
  for (iS in seq_along(template_sheets[[idx]])) {
    sheetName <- template_sheets[[idx]][[iS]]
    tryCatch({
      df <- as.data.frame(
        readxl::read_excel(
          template_file[[idx]],
          sheet = iS,
          col_names = paste0("x", seq_len(plate_info$n_col)),
          range = plate_info$plate_range
        )
      )
    }, error = function(e) {
      exception_data <- get_exception_data(20)
      stop(sprintf(exception_data$sprintf_text,
                   template_file[[idx]],
                   e))
    })
    df$WellRow <- LETTERS[seq_len(plate_info$n_row)]
    df_melted <- reshape2::melt(df, id.vars = "WellRow")
    # check if metadata field already exist and correct capitalization if needed
    if (!(sheetName %in% metadata_fields)) {
      if (!is.null(metadata_fields) &&
          toupper(sheetName) %in% toupper(metadata_fields)) {
        oldiS <- sheetName
        sheetName <-
          metadata_fields[toupper(sheetName) == toupper(metadata_fields)]
        futile.logger::flog.info("%s corrected to match case with %s", oldiS, sheetName)
      } else {
        metadata_fields <- c(metadata_fields, sheetName)
      }
    }
    colnames(df_melted)[3] <- sheetName
    colnames(df_melted)[colnames(df_melted) == "variable"] <-
      "WellColumn"
    df_melted$WellColumn <-
      gsub("x", "", df_melted$WellColumn)
    df_template <-
      base::merge(df_template, df_melted, by = c("WellRow", "WellColumn"))
  }
  df_template
}

#' Get plate info from template xlsx
#' 
#' @param template_file character, file path(s) to template(s)
#' @param Gnumber_idx index with Gnumber data
#' @param idx template file index
#' 
get_plate_info_from_template_xlsx <- function(template_file, Gnumber_idx, idx) {
  
      tryCatch({
        df <-
          readxl::read_excel(
            template_file[[idx]],
            sheet = Gnumber_idx,
            col_names = paste0("x", seq_len(48)),
            range = "A1:AV32",
            col_types = "text"
          )
      }, error = function(e) {
        exception_data <- get_exception_data(5)
        stop(sprintf(exception_data$sprintf_text, e))
      })

      # get the plate size
      n_row <-
        2 ^ ceiling(log2(max(which(
          apply(!is.na(df), 1, any)
        ))))
      n_col <-
        1.5 * 2 ^ ceiling(log2(max(which(
          apply(!is.na(df), 2, any)
        )) / 1.5))
      n_row <- max(n_row, n_col / 1.5)
      n_col <- max(1.5 * n_row, n_col)
      list(
        plate_range = ifelse(n_col < 26, paste0("A1:", LETTERS[n_col], n_row), "A1:AV32"),
        n_row = n_row,
        n_col = n_col
      )
}

#' Validate template xlsx data
#' 
#' @param template_file character, file path(s) to template(s)
#' @param template_filename character, file name(s)
#' @param template_sheets template sheet names
#' @param idx template file index
#' 
validate_template_xlsx <- function(template_file, template_filename, template_sheets, idx) {
  
      # first check that the sheet names are ok
      # identify drug_identifier sheet
      Gnumber_idx <- which(template_sheets[[idx]] %in% gDRutils::get_env_identifiers("drug"))
      Conc_idx <-
        grepl("Concentration", template_sheets[[idx]], ignore.case = TRUE)
      # case of untreated plate
      if (sum(Conc_idx) == 0) {
        if (length(Gnumber_idx) == 0) {
          exception_data <- get_exception_data(17)
          stop(sprintf(
            exception_data$sprintf_text,
            template_file[[idx]],
            gDRutils::get_env_identifiers("drug")
          ))
        }
        tryCatch({
          df <-
            readxl::read_excel(
              template_file[[idx]],
              sheet = Gnumber_idx,
              col_names = paste0("x", seq_len(48)),
              range = "A1:AV32"
            )
        }, error = function(e) {
          exception_data <- get_exception_data(5)
          stop(sprintf(exception_data$sprintf_text, e))
        })
        if (!(all(toupper(unlist(df)[!is.na(unlist(df))]) %in%
                  toupper(gDRutils::get_env_identifiers(
                    "untreated_tag"
                  ))))) {
          exception_data <- get_exception_data(18)
          stop(sprintf(
            exception_data$sprintf_text,
            template_file[[idx]],
            paste(gDRutils::get_env_identifiers("untreated_tag"), collapse = " or ")
          ))
        }
      } else {
        # normal case
        dump <- check_metadata_names(template_sheets[[idx]],
                                     df_name = template_filename[idx],
                                     df_type = "template_treatment")
      }
} 

#' Load results from tsv
#'
#' This functions loads and checks the results file(s)
#'
#' @param results_file character, file path(s) to template(s)
#' @param headers list of headers identified in the manifest
#'
load_results_tsv <-
  function(results_file, headers) {
    
    # results_file is a string or a vector of strings
    results_filename <- basename(results_file) 

    all_results <- read_in_result_files(results_file, results_filename, headers)

    if (dim(unique(df[, c(headers[["barcode"]], gDRutils::get_env_identifiers("well_position"))]))[1] !=
        dim(df[, c(headers[["barcode"]], gDRutils::get_env_identifiers("well_position"))])[1]) {
      futile.logger::flog.error("Multiple rows with the same Barcode and Well across all files")
    }

    return(all_results)
  }

read_in_result_files <- function(results_file, results_filename, headers) {
  
    # read all files
    res_l <- lapply(results_file, function(iF) {
      futile.logger::flog.info("Reading file", results_file[iF])
      tryCatch({
        df <-
        utils::read.table(results_file[iF], sep = "\t", header = TRUE, na.strings = c("", "NA")) %>%
          stats::na.omit()
      }, error = function(e) {
        exception_data <- get_exception_data(21)
        stop(sprintf(exception_data$sprintf_text, results_file[[iF]]))
      })
      # skip_empty_rows flag needs to be TRUE even if it ends up not skipping empty rows
      if (ncol(df) == 1) {
        tryCatch({
          # likely a csv file
          df <-
            utils::read.csv(results_file[iF], header = TRUE, na.strings = c("", "NA")) %>%
            stats::na.omit()
        }, error = function(e) {
          exception_data <- get_exception_data(21)
          stop(sprintf(exception_data$sprintf_text, results_file[[iF]]))
        })
      }
      for (coln in c(headers[["barcode"]],
                     gDRutils::get_env_identifiers("well_position"),
                     "ReadoutValue")) {
        if (!(coln %in% colnames(df))) {
          futile.logger::flog.error("%s needs to be a column of %s", coln, results_filename[iF])
        }
      }
      if (dim(unique(df[, c(headers[["barcode"]], gDRutils::get_env_identifiers("well_position"))]))[1] !=
          dim(df[, c(headers[["barcode"]], gDRutils::get_env_identifiers("well_position"))])[1]) {
        futile.logger::flog.error("Multiple rows with the same Barcode and Well in %s",
                                  results_filename[iF])
      }
      if (!("BackgroundValue" %in% colnames(df)))
        df$BackgroundValue <- 0
      futile.logger::flog.info("File %s read; %d wells", results_filename[iF], nrow(df))
      futile.logger::flog.info("File done")
    })
    all_results <- do.call(rbind, res_l)
}

#' Load envision results from xlsx
#'
#' This functions loads and checks the results file(s)
#'
#' @param results_file character, file path(s) to results file(s)
#' @param headers list of headers identified in the manifest
load_results_EnVision <-
  function(results_file, headers = gDRutils::get_env_identifiers()) {
    # Assertions:
    checkmate::assert_character(results_file)

    results_filename <- basename(results_file)
    # results_file is a string or a vector of strings
    # test if the result files are .tsv or .xls(x) files
    isExcel <- vapply(results_file, function(x) {
      return(tools::file_ext(x) %in% c("xlsx", "xls"))
    },
    logical(1))

    # read sheets in files; warning if more than one sheet (unexpected but can be handled)
    results_sheets <- vector("list", length(results_file))
    results_sheets[!isExcel] <- 0
    results_sheets[isExcel] <-
      lapply(results_file[isExcel], readxl::excel_sheets)
    if (any(lapply(results_sheets, length) > 1)) {
      futile.logger::flog.warn("Multiple sheets in result file: %s",
                               results_file[lapply(results_sheets, length) > 1])
    }

    # read all files and sheets
    all_results <- data.frame()
    for (iF in seq_along(results_file)) {
      for (iS in results_sheets[[iF]]) {
        futile.logger::flog.info("Reading file %s, sheet %s", results_file[[iF]], iS)
        if (iS == 0) {
          fInfo <- read_EnVision(results_file[[iF]])
          df <- fInfo$df
          isEdited <- fInfo$isEdited
        } else {
          isEdited <- TRUE
          # expect an Excel spreadsheet
          exception_data <- get_exception_data(22)
          if (length(results_sheets[[iF]]) > 1) {
            # if multiple sheets, assume 1 plate per sheet
            tryCatch({
              df <-
                readxl::read_excel(
                  results_file[[iF]],
                  sheet = iS,
                  col_names = paste0("x", seq_len(48)),
                  range = "A1:AV32"
                )
            }, error = function(e) {
              stop(sprintf(exception_data$sprintf_text, results_file[[iF]], iS))
            })
          } else {
            tryCatch({
              df <- readxl::read_excel(results_file[[iF]],
                                       sheet = iS,
                                       col_names = FALSE)
            }, error = function(e) {
              stop(sprintf(exception_data$sprintf_text, results_file[[iF]], iS))
            })
            colnames(df) <-
              col_names <- paste0("x", seq_len(ncol(df)))
          }
          # Find rows with data and drop empty columns
          colsRange <- grep("Plate information", unlist(df[, 1]))[1] + 10
          df <- df[, colSums(is.na(df[seq_len(colsRange), ])) != colsRange]

          # remove extra columns
          # limit to first 24 rows in case Protocol information is
          # exported which generate craps at the end of the file
        }

        # not empty rows
        # before discarding the rows; move ''Background information'' in the next row
        Bckd_info_idx <-
          which(as.data.frame(df)[, 1] %in% "Background information")
        if (length(Bckd_info_idx) > 0) {
          df[Bckd_info_idx + 1, 1] <- df[Bckd_info_idx, 1]
          df[Bckd_info_idx, 1] <- ""
        }

        if (isEdited) {

          # get the expected plate size
          plate_dim <- .get_plate_size(df)
          n_row <- plate_dim[1]
          n_col <- plate_dim[2]

          # manually add full rows
          plate_rows <- which(do.call(paste, df[, c(2, 3)]) %in% "Repeat Barcode") - 1
          spacer_rows <- grep("[[:alpha:]]", as.data.frame(df)[, 1])

          standardized_bckd_info <- if (length(Bckd_info_idx) == 0) {
            0
          } else {
            Bckd_info_idx
          }

          actual_plate_rows <- pmax(plate_rows, standardized_bckd_info)

          data_rows <- unlist(lapply(actual_plate_rows,
                                     function(x) (x + 4):(x + 4 + n_row - 1)))

          # find full numeric rows
          df <- .fill_empty_wells(df, plate_rows, data_rows, n_row, n_col)


          # need to do some heuristic to find where the data is
          df_to_check <- df[, -6:-1]
          full_rows <- Reduce(union, lapply(df, function(x) grep("^\\d+$", x)))

          barcode_col <- grep(paste0(headers[["barcode"]], collapse = "|"), as.data.frame(df))[1]
          Barcode_idx <-
            which(unlist(as.data.frame(df)[, barcode_col]) %in% headers[["barcode"]])

          additional_rows <- c(Barcode_idx, Bckd_info_idx + 1)

          full_rows_index <- unique(sort(c(additional_rows, additional_rows + 1,
                                    setdiff(full_rows, spacer_rows))))

          # don't consider the first columns as these may be metadata
          # if big gap, delete what is at the bottom (Protocol information)
          gaps <-
            min(max(data_rows), nrow(df))

          df <-
            df[full_rows_index[full_rows_index <= gaps], ]

          Barcode_idx <-
            which(unlist(as.data.frame(df)[, barcode_col]) %in% headers[["barcode"]])

          # add empty column to complete plate (assume left column is #1)
          if (ncol(df) < n_col) {
            df[, (ncol(df) + 1):n_col] <- NA
          }


          # run through all plates
          for (iB in Barcode_idx) {
            # two type of format depending on where Background information is placed
            if (any(as.data.frame(df)[iB + (seq_len(4)), 1] %in% "Background information")) {
              ref_bckgrd <-
                which(as.data.frame(df)[iB + (seq_len(4)), 1] %in% "Background information")
              readout_offset <- 1 + ref_bckgrd
              stopifnot(as.character(df[iB + ref_bckgrd, 4]) %in% "Signal")
              BackgroundValue <-
                as.numeric(df[iB + ref_bckgrd + 1, 4])
            } else {
              # export without background information
              # case of " Exported with EnVision Workstation version 1.13.3009.1409 "
              ref_bckgrd <- 0
              readout_offset <- 1
              BackgroundValue <- 0
            }

            # check the structure of file is ok
            .check_file_structure(df, iB, iF, iS,
                                  results_filename, readout_offset,
                                  n_row, n_col, barcode_col)

            Barcode <- as.character(df[iB + 1, barcode_col])
            if (is.na(Barcode)) next
            readout <-
              as.matrix(df[iB + ref_bckgrd + seq_len(n_row) + 1, seq_len(n_col)])

            stopifnot(dim(readout) == plate_dim)

            # check that the plate size is consistent and contains values
            if (any(is.na(readout))) {
              exception_data <- get_exception_data(23)
              stop(
                sprintf(
                  exception_data$sprintf_text,
                  results_filename[[iF]],
                  iS,
                  as.character(df[iB + 1, barcode_col])
                )
              )
            }

            df_results <- data.frame(
              Barcode = Barcode,
              WellRow = LETTERS[seq_len(n_row)],
              WellColumn = as.vector(t(matrix(
                seq_len(n_col), n_col, n_row
              ))),
              ReadoutValue = as.numeric(as.vector(readout)),
              BackgroundValue = BackgroundValue
            )
            names(df_results)[1] <- headers[["barcode"]][1]

            futile.logger::flog.info("Plate %s read; %d wells",
                                     as.character(df[iB + 1, barcode_col]),
                                     dim(df_results)[1])
            all_results <- rbind(all_results, df_results)
            }
          } else {
            # proper original EnVision file
            n_row <- fInfo$n_row
            n_col <- fInfo$n_col
            Barcode <- df[3, barcode_col]
            readout <-
              as.matrix(df[4 + seq_len(n_row), seq_len(n_col)])

            if (any(as.data.frame(df)[, 1] %in% "Background information")) {
              ref_bckgrd <-
                which(as.data.frame(df)[, 1] %in% "Background information")
              BackgroundValue <-
                as.numeric(df[ref_bckgrd + 1, 4])
            } else {
              # export without background information
              BackgroundValue <- 0
            }

          df_results <- data.frame(
            Barcode = Barcode,
            WellRow = LETTERS[seq_len(n_row)],
            WellColumn = as.vector(t(matrix(
              seq_len(n_col), n_col, n_row
            ))),
            ReadoutValue = as.numeric(as.vector(readout)),
            BackgroundValue = BackgroundValue
          )
          futile.logger::flog.info("Plate %s read; %d wells",
                                   as.character(df[iB + 1, barcode_col]),
                                   dim(df_results)[1])

          all_results <- rbind(all_results, df_results)
        }

      }
      futile.logger::flog.info("File done")
    }
    return(all_results)
  }


#' Load tecan results from xlsx
#'
#' This functions loads and checks the results file
#'
#' @param results_file string, file path to a result file
#' @param headers list of headers identified in the manifest
load_results_Tecan <-
  function(results_file, headers = gDRutils::get_env_identifiers()) {
    # Assertions:
    checkmate::assert_string(results_file)
    # check the result files is a .xls(x) file
    isExcel <- tools::file_ext(results_file) %in% c("xlsx", "xls")
    if (!isExcel) {
      futile.logger::flog.error("Results file for Tecan importer must be a 'xls(x)' file: %s",
                                results_file)
    }
    # read sheets in files; in the Tecan format each sheet is a plate
    results_sheets <- readxl::excel_sheets(results_file)
    if (length(results_sheets) < 1) {
      futile.logger::flog.error("No data sheet found in: %s",
                                results_file)
    }
    # # read all plates
    all_results <- read_in_results_Tecan(results_file, results_sheets, headers)
    return(all_results)
  }

#' read in Tecan data
#' 
#' @param results_file string, file path to a result file
#' @param results_sheets template sheet names
#' @param headers list of headers identified in the manifest
read_in_results_Tecan <- function(results_file, results_sheets, headers) {
  all_results <- data.frame()
  for (iS in seq_along(results_sheets)) {
    futile.logger::flog.info("Reading file %s, sheet %s", results_file, results_sheets[[iS]])
    # read the content of each plate
    tryCatch({
      df <- readxl::read_excel(results_file,
                               sheet = results_sheets[[iS]],
                               col_names = FALSE)
    }, error = function(e) {
      exception_data <- get_exception_data(22)
      stop(sprintf(exception_data$sprintf_text, results_file, iS))
    })
    
    # find the indicator ("<>") that identifies where plate readings are
    ind <- which(df == "<>", arr.ind = TRUE)
    dfm <- df[(ind[1]):nrow(df), ind[2]:ncol(df)] # remove text above "<>"
    # remove text after data matrix ends, as identified by first na value
    ind <- which(is.na(dfm), arr.ind = TRUE)[1]
    dfm <- dfm[seq_len(ind) - 1, seq_len(ncol(dfm))]
    
    # rows and columns in data matrix with row and col names
    n_row <- nrow(dfm)
    n_col <- ncol(dfm)
    readout <- as.data.frame(dfm[2:n_row, 2:n_col])
    rownames(readout) <- t(dfm[2:n_row, 1])
    colnames(readout) <- dfm[1, 2:n_col]
    # rows and columns in readout matrix
    n_row <- nrow(readout)
    n_col <- ncol(readout)
    # get well identifiers (numbers and letters) from layout
    WellRow <- rownames(readout)
    WellColumn <- strtoi(colnames(readout))
    
    # results data frame for plate
    df_results <- data.frame(
      Barcode = results_sheets[iS],
      WellRow = WellRow,
      WellColumn =  as.vector(t(matrix(
        WellColumn, n_col, n_row
      ))),
      ReadoutValue = as.numeric(as.vector(as.matrix(readout))),
      BackgroundValue = 0 ## Tecan users report negligible background readings, usually background is not recorded
    )
    names(df_results)[1] <- headers[["barcode"]]
    all_results <- rbind(all_results, df_results)
  }
  all_results
}

#' check_metadata_names
#'
#' Check whether all metadata names are correct
#'
#' @param col_df a character with colnames of df
#' @param df_name a name of dataframe ("" by default)
#' @param df_type a type of a dataframe (NULL by default)
#'
#' @return corrected names
#' @export
#'
check_metadata_names <-
  function(col_df,
           df_name = "",
           df_type = NULL) {
    # Assertions:
    checkmate::assert_character(col_df)
    checkmate::assert_character(df_name)
    checkmate::assert_character(df_type, null.ok = TRUE)

    check_metadata_req_col_names(col_df, df_name, df_type)
    corrected_names <- col_df
    corrected_names <- check_metadata_against_spaces(corrected_names, df_name)
    corrected_names <- check_metadata_field_names(corrected_names, df_name)
    corrected_names <- check_metadata_headers(corrected_names, df_name)
    corrected_names
  }

#' Check metadata for required column names
# 
#' @param col_df a charvec with corrected colnames of df
#' @param df_name a name of dataframe ("" by default)
#' @param df_type a type of a dataframe (NULL by default)
#' 
check_metadata_req_col_names <- function(col_df, df_name, df_type) {
  # first check for required column names
  if (!is.null(df_type)) {
    if (df_type == "manifest") {
      expected_headers <- gDRutils::get_header("manifest")
    } else if (df_type == "template") {
      expected_headers <- gDRutils::get_env_identifiers("drug")
    } else if (df_type == "template_treatment") {
      expected_headers <-
        c(gDRutils::get_env_identifiers("drug"),
          "Concentration")
    }
    upperHeaders <- lapply(expected_headers, toupper)
    headersOK <-
      vapply(upperHeaders, function(x)
        any(x %in% toupper(col_df)), FUN.VALUE = logical(1))
    if (any(!headersOK)) {
      exception_data <- get_exception_data(24)
      stop(sprintf(
        exception_data$sprintf_text, df_type,
        toString(expected_headers[!(expected_headers %in% col_df)])))
    }
    if (df_type == "template_treatment") {
      # assess if multiple drugs and proper pairing
      n_drug <- agrep(gDRutils::get_env_identifiers("drug"), col_df, ignore.case = TRUE)
      n_conc <- agrep("Concentration", col_df, ignore.case = TRUE)
      if (length(n_drug) != length(n_conc)) {
        exception_data <- get_exception_data(32)
        stop(sprintf(exception_data$sprintf_text, df_name))
      }
      if (length(n_drug) > 1) {
        trt_sheets <- c(
          paste0(
            gDRutils::get_env_identifiers("drug"), "_", 2:length(n_drug)),
          paste0("Concentration_", 2:length(n_conc))
        )
        if (!(all(toupper(trt_sheets) %in% toupper(col_df)))) {
          exception_data <- get_exception_data(25)
          stop(sprintf(
            exception_data$sprintf_text, df_name,
            toString(trt_sheets[!(toupper(trt_sheets) %in% toupper(col_df))])))
        }
      }
    }
  }
  
}

#' Check metadata field names
# 
#' @param corrected_names a charvec with corrected colnames of df
#' @param df_name a name of dataframe ("" by default)
#' 
check_metadata_field_names <- function(corrected_names, df_name) {
  # check for wrong metadata field names (including dash, starting with number, ... )
  bad_names <-
    regexpr("\\W", corrected_names) > 0 |
    regexpr("\\d", corrected_names) == 1
  if (any(bad_names)) {
    exception_data <- get_exception_data(26)
    stop(sprintf(
      exception_data$sprintf_text,
      df_name,
      toString(corrected_names[bad_names])
    ))
  }
  corrected_names
}

#' Check metadata against spaces
# 
#' @param corrected_names a charvec with corrected colnames of df
#' @param df_name a name of dataframe ("" by default)
#' 
check_metadata_against_spaces <- function(corrected_names, df_name) {
  # remove spaces and convert to WordUppercase
  names_spaces <- regexpr("\\s", corrected_names) > 0
  if (any(names_spaces)) {
    for (i in which(names_spaces)) {
      s <- strsplit(corrected_names[i], " ")[[1]]
      corrected_names[i] <-
        paste(toupper(substring(s, 1, 1)),
              substring(s, 2),
              sep = "",
              collapse = "")
    }
    futile.logger::flog.warn(
      "Metadata field names for %s cannot contain spaces --> corrected to: %s",
      df_name,
      toString(corrected_names[names_spaces])
    )
  }
  corrected_names
}

#' Check whether metadata headers are correct and make fixes if needed
# 
#' @param corrected_names a charvec with corrected colnames of df
#' @param df_name a name of dataframe ("" by default)
#' 
check_metadata_headers <- function(corrected_names, df_name) {
  
  # common headers that are written in a specific way
  # throw warning if close match and correct upper/lower case for consistency
  controlled_headers <- gDRutils::get_header("controlled")
  for (i in seq_along(controlled_headers)) {
    grep_pattern <- paste0(controlled_headers[[i]], "$", collapse = "|")
    exact_match_grep <- grep(grep_pattern, corrected_names)
    
    # To avoid cases when grep compare 'PLATE' to 'temPLATE'
    case_match <- setdiff(grep(grep_pattern, corrected_names, ignore.case = TRUE),
                          exact_match_grep)
    if (isTRUE(length(exact_match_grep) == 1 ||
               corrected_names[case_match] %in% controlled_headers))
      next
    
    if (length(case_match) > 0) {
      corrected_names[case_match] <- controlled_headers[[i]]
      futile.logger::flog.warn("Header %s in %s corrected to %s",
                               corrected_names[case_match],
                               df_name,
                               controlled_headers[[i]])
    }
  }
  
  # check for headers that are reserved for downstream analyses
  check_headers <-
    setdiff(
      gDRutils::get_header("reserved"),
      gDRutils::get_env_identifiers("well_position")
    )
  if (any(corrected_names %in% check_headers)) {
    exception_data <- get_exception_data(27)
    stop(sprintf(
      exception_data$sprintf_text,
      toString(intersect(check_headers, corrected_names)),
      df_name
    ))
  }
  corrected_names
}

#' Read envision files
#'
#' This function reads file from the EnVision Workstation
#'
#' @param file  input file from EnVision
#' @param nrows maximum number of file rows to be processed
#' @param seps potential field separators of the input file
#'
#' @return a list containing the data frame, n_col, n_row, and if is edited
#' @export
read_EnVision <- function(file,
                         nrows = 10000,
                         seps = c(",", "\t")) {
  # Assertions:
  checkmate::assert_string(file)
  checkmate::assert_number(nrows)
  checkmate::assert_character(seps)
  con <- file(file)
  open(con)

  results.list <- list()
  results_file <- NULL
  iF <- NULL

  current.line <- 1
  while (length(line <-
                readLines(con, n = 1, warn = FALSE)) > 0 & current.line < nrows) {
    results.list[[current.line]] <- line
    current.line <- current.line + 1
  }
  close(con)

  n_sep <- colSums(vapply(seps, function(sep) {
    vapply(results.list[seq_len(10)], function(line)
      length(unlist(strsplit(line, split = sep))),
      integer(length = 1))
  }, integer(length = 10)))
  sep <- n_sep [n_sep == max(n_sep)]
  if (sep < 30) {
    exception_data <- get_exception_data(28)
    stop(sprintf(exception_data$sprintf_text, file))
  }
  sep <- names(sep)

  results.list <-
    lapply(results.list, function(line)
      unlist(strsplit(line, split = sep)))

  # identify if original csv file or not
  if (which(vapply(results.list, function(x)
    grepl("Plate information", x[1]), logical(1))) != 1) {
    # fails if not the right header
    exception_data <- get_exception_data(29)
    stop(sprintf(
      exception_data$sprintf_text,
      results_file[[iF]]
    ))
  }
  # has been open/saved in a spreadsheet software --> first line is padded with empty columns
  isEdited <- (length(results.list[[1]]) > 1) |
    !any(vapply(results.list, function(x)
      grepl("EnVision Workstation", x[1]), logical(1)))

  if (isEdited) {
    # may be altered and miss columns
    n_col <- max(vapply(results.list[5:10], length, integer(length =
                                                             1)))
    n_col <- 1.5 * 2 ^ ceiling(log2((n_col - 1) / 1.5))
    n_row <- which(vapply(results.list, function(x) {
      grepl("Basic assay information", x[1]) |
        grepl("Plate information", x[1])
    },
    logical(1)))
    if (length(n_row) == 1 && n_row == 1) {
      # only the top line with "Plate information" was found
      n_row <- length(results.list) - 4
    } else {
      # scrap a few rows above "Basic assay information"
      n_row <- min(n_row[n_row > 1]) - 7
    }
    if (length(n_row) != 1 ||
        abs(log2(1.5 * n_row / n_col)) > .5)  {
      exception_data <- get_exception_data(30)
      stop(sprintf(exception_data$sprintf_text, results_file[[iF]]))
    }
    n_row <- n_col / 1.5
  } else {
    n_col <- max(vapply(results.list[5:10], length, integer(length = 1)))
    n_row <- sum(vapply(results.list[5:which(vapply(results.list, function(x)
      grepl("Basic assay information", x[1]), logical(1)))],
      length, integer(1)) == n_col)
    if (log2(1.5 * n_row / n_col) != 0)  {
      exception_data <- get_exception_data(30)
      stop(sprintf(exception_data$sprintf_text, results_file[[iF]]))
    }
  }

  # pad the lines with NA if not full before creating the dataframe
  results.list <- lapply(results.list, function(x) {
    if (length(x) < n_col) {
      x <- c(x, array(NA, n_col - length(x)))
    } else if (length(x) > n_col) {
      x <- x[seq_len(n_col)]
    }
    x[x == "" & !is.na(x)] <- NA
    return(x)
  })
  df_ <- as.data.frame(do.call(rbind, results.list), stringsAsFactors = FALSE)
  colnames(df_) <- paste0("x", seq_len(n_col))

  rownames(df_) <- NULL
  return(list(
    df = df_,
    n_col = n_col,
    n_row = n_row,
    isEdited = isEdited
  ))
}

#' Get plate size
#' @keywords internal
.get_plate_size <- function(df) {
  n_col <-
    1.5 * 2 ^ ceiling(log2((ncol(df) - 2) / 1.5))
  n_row <- n_col / 1.5
  c(n_row, n_col)
}


#' Check the structure of raw data
#' @keywords internal
.check_file_structure <- function(df, iB, iF, iS, results_filename,
                                  readout_offset, n_row, n_col, barcode_col) {
  # check the structure of file is ok
  check_values <-
    as.matrix(df[iB + readout_offset + c(0, 1, n_row, n_row + 1), n_col])
  if (is.na(check_values[2])) {
    exception_data <- get_exception_data(31)
    stop(
      sprintf(
        exception_data$sprintf_text,
        results_filename[[iF]],
        iS,
        as.character(df[iB + 1, barcode_col])
      )
    )
  }
}


#' Correct plates with not fully filled readout values
#' @keywords internal
.fill_empty_wells <- function(df, plate_rows, data_rows, exp_row, exp_col, numeric_regex = "^\\d+$") {
  all_rows <- Reduce(intersect, lapply(df, function(x) grep(numeric_regex, x)))
  if (ncol(df) < exp_col) {
    new_cols <- exp_col - ncol(df)
    df <- cbind(df, matrix(ncol = new_cols))
  }
  if (length(all_rows) / exp_row != length(plate_rows)) {
    fill_rows <- intersect(which(apply(df, 1, function(x) all(is.na(x)))), data_rows)
    df[fill_rows, ] <- "0"

    #fill up data_rows
    for (i in data_rows) {
      df[i, c(is.na(df[i, ]))] <- "0"
    }
  }
  df
}


#' Standardize untreated values to ignore cases
#' @keywords internal
.standardize_untreated_values <- function(df) {
  untreated_tags <- gDRutils::get_env_identifiers("untreated_tag")
  as.data.frame(lapply(df, function(x) {
    if (is.factor(x)) x <- as.character(x)
    x[toupper(x) %in% toupper(untreated_tags)] <- untreated_tags[[1]]
    return(x)
    }))
}
