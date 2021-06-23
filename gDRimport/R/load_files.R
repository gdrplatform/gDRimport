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
      # for the shiny app
      template_file <- df_template_files$datapath
      is_readable_v(template_file)
      template_filename <- df_template_files$name
    } else {
      template_filename <- df_template_files
      is_readable_v(template_filename)
    }
    
    manifest <- load_manifest(manifest_file)
    treatments <- load_templates(df_template_files)
    data <- load_results(results_file, instrument)
    
    # check the all template files are available
    if (!all(unique(manifest$Template[manifest$Barcode %in% data$Barcode])
             %in% basename(template_filename))) {
      stop(sprintf(
        "Some template files are missing: %s",
        toString(setdiff(
          unique(manifest$Template[manifest$Barcode %in% data$Barcode]),
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
  
  # Asserts:
  assertthat::assert_that(is.character(manifest_file), msg = "'manifest_file' must be a character vector")
  
  # read files
  manifest_data <- lapply(manifest_file, function(x) {
    manifest_ext <- tools::file_ext(x)
    if (manifest_ext %in% c("xlsx", "xls")) {
      df <- tryCatch({
        readxl::read_excel(x, col_names = TRUE)
      }, error = function(e) {
        stop(sprintf(
          "Error reading the Manifest file. Please see the logs:\n%s",
          e
        ))
      })
    } else if (manifest_ext %in% c("text/tsv",
                                   "text/tab-separated-values",
                                   "tsv")) {
      df <- tryCatch({
        utils::read.table(x, sep = "\t", header = TRUE, na.strings=c("", "NA")) %>%
          stats::na.omit()
      }, error = function(e) {
        stop(sprintf(
          "Error reading the Manifest file. Please see the logs:\n%s",
          e
        ))
      })
    } else {
      stop(
        sprintf(
          "%s file format is not supported.
          Please convert your file to one of the following: %s",
          manifest_ext,
          stringi::stri_flatten(available_formats, collapse = ", ")
        )
      )
    }
  })
  
  # replace Time by Duration for backwards compatibility
  manifest_data <- lapply(manifest_data, function(x) {
    if ("Time" %in% colnames(x)) {
      colnames(x)[colnames(x) == "Time"] <-
        gDRutils::get_identifier("duration")
    }
    return(x)
  })
  
  # check default headers are in each df
  dump <- sapply(1:length(manifest_file),
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
  if (dim(cat_manifest_data)[1] != length(unique(cat_manifest_data$Barcode)))
    stop("Barcodes in Manifest must be unique!")
  
  cat_manifest_data$Template <- basename(cat_manifest_data$Template)
  
  futile.logger::flog.info("Manifest loaded successfully")
  return(cat_manifest_data)
}


#' Load templates
#'
#' This functions loads and checks the template file(s)
#'
#' @param df_template_files data.frame, with datapaths and names of results file(s)
#' or character with file path of templates file(s)
#' @export
load_templates <- function (df_template_files) {
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
  
  all_templates$Gnumber <- standardize_record_values(all_templates$Gnumber, dictionary = DICTIONARY)
  
  return(all_templates)
  
}


#' Load results
#'
#' This functions loads and checks the results file(s)
#'
#' @param df_results_files  data.frame, with datapaths and names of results file(s)
#' or character with file path of results file(s)
#' @param instrument character
#' @export
#'
load_results <-
  function(df_results_files, instrument = "EnVision") {
    stopifnot(any(inherits(df_results_files, "data.frame"), checkmate::test_character(df_results_files)))
    checkmate::assert_string(instrument, pattern = "^EnVision$|^long_tsv$")
    if (is.data.frame(df_results_files)) {
      # for the shiny app
      results_file <- df_results_files$datapath
      results_filename <- df_results_files$name
    } else {
      results_file <- df_results_files
      results_filename <- basename(results_file)
    }
    stopifnot(sapply(results_file, file.exists))
    
    if (instrument == "EnVision") {
      all_results <-
        load_results_EnVision(results_file)
    } else if (instrument == "long_tsv") {
      all_results <-
        load_results_tsv(results_file)
    }
    return(all_results)
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
      utils::read.table(x, sep = "\t", header = TRUE, na.strings=c("", "NA")) %>%
        stats::na.omit())
    names(templates) <- template_filename
    # check WellRow/WellColumn is present in each df
    dump <- sapply(1:length(template_file),
                   function(i)
                     if (!(all(
                       gDRutils::get_identifier("well_position") %in% colnames(templates[[i]])
                     ))) {
                       futile.logger::flog.info("%s missing, %s as header",
                                                template_filename[[i]],
                                                gDRutils::get_identifier("well_position"))
                     })
    # check drug_identifier is present in each df
    dump <- sapply(1:length(template_file),
                   function(i)
                     check_metadata_names(
                       setdiff(colnames(templates[[i]]), gDRutils::get_identifier("well_position")),
                       df_name = template_filename[[i]],
                       df_type = "template"
                     ))
    
    metadata_fields <- NULL
    all_templates <- data.frame()
    for (iF in 1:length(template_file)) {
      futile.logger::flog.info("Loading %s", template_filename[iF])
      # first check that the sheet names are ok
      # identify drug_identifier sheet (case insensitive)
      Gnumber_idx <- grep(paste0(gDRutils::get_identifier("drug"), "$"),
                          colnames(templates[[iF]]),
                          ignore.case = TRUE)
      Conc_idx <-
        grepl("Concentration", colnames(templates[[iF]]), ignore.case = TRUE)
      # case of untreated plate
      if (sum(Conc_idx) == 0) {
        if (length(Gnumber_idx) == 0) {
          stop(sprintf(
            "In untreated template file %s, sheet name must be %",
            template_file[[iF]],
            gDRutils::get_identifier("drug")
          ))
        }
        df <- templates[[iF]][, gDRutils::get_identifier("drug")]
        if (!(all(toupper(df)[!is.na(df)]) %in% toupper(gDRutils::get_identifier("untreated_tag")))) {
          stop(sprintf(
            "In untreated template file %s, entries must be %s",
            template_file[[iF]],
            paste(gDRutils::get_identifier("untreated_tag"), collapse = " or ")
          ))
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
        check_metadata_names(colnames(df_template),
                             df_name = template_filename[iF])
      all_templates <- rbind(all_templates, df_template)
      
    }
    futile.logger::flog.info("Templates loaded successfully!")
    return(all_templates)
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
    # read sheets in files
    template_sheets <- lapply(template_file, readxl::excel_sheets)
    # check drug_identifier is present in each df
    dump <- sapply(1:length(template_file),
                   function(i)
                     check_metadata_names(
                       template_sheets[[i]],
                       df_name = template_file[[i]],
                       df_type = "template"
                     ))
    
    metadata_fields <- NULL
    all_templates <- data.frame()
    for (iF in 1:length(template_file)) {
      futile.logger::flog.info("Loading", template_filename[iF])
      # first check that the sheet names are ok
      # identify drug_identifier sheet (case insensitive)
      Gnumber_idx <- grep(paste0(gDRutils::get_identifier("drug"), "$"),
                          template_sheets[[iF]],
                          ignore.case = TRUE)
      Conc_idx <-
        grepl("Concentration", template_sheets[[iF]], ignore.case = TRUE)
      # case of untreated plate
      if (sum(Conc_idx) == 0) {
        if (length(Gnumber_idx) == 0) {
          stop(sprintf(
            "In untreated template file %s, sheet name must be %",
            template_file[[iF]],
            gDRutils::get_identifier("drug")
          ))
        }
        tryCatch({
          df <-
            readxl::read_excel(
              template_file[[iF]],
              sheet = Gnumber_idx,
              col_names = paste0("x", 1:48),
              range = "A1:AV32"
            )
        }, error = function(e) {
          stop(sprintf("Error loading template. See logs: %s", e))
        })
        if (!(all(toupper(unlist(df)[!is.na(unlist(df))]) %in%
                  toupper(gDRutils::get_identifier(
                    "untreated_tag"
                  ))))) {
          stop(sprintf(
            "In untreated template file %s, entries must be %s",
            template_file[[iF]],
            paste(gDRutils::get_identifier("untreated_tag"), collapse = " or ")
          ))
        }
      } else {
        # normal case
        dump <- check_metadata_names(template_sheets[[iF]],
                                     df_name = template_filename[iF],
                                     df_type = "template_treatment")
      }
      # read the different sheets and check for plate size
      # enforce range to avoid skipping empty rows at the beginning
      tryCatch({
        df <-
          readxl::read_excel(
            template_file[[iF]],
            sheet = template_sheets[[iF]][Gnumber_idx],
            col_names = paste0("x", 1:48),
            range = "A1:AV32",
            col_types = "text"
          )
      }, error = function(e) {
        stop(sprintf("Error loading template. See logs: %s", e))
      })
      # get the plate size
      n_row <-
        2 ** ceiling(log2(max(which(
          apply(!is.na(df), 1, any)
        ))))
      n_col <-
        1.5 * 2 ** ceiling(log2(max(which(
          apply(!is.na(df), 2, any)
        )) / 1.5))
      n_row <- max(n_row, n_col / 1.5)
      n_col <- max(1.5 * n_row, n_col)
      plate_range <-
        ifelse(n_col < 26, paste0("A1:", LETTERS[n_col], n_row), "A1:AV32")
      
      # need to adapt for 1536 well plates
      df_template <-
        base::expand.grid(WellRow = LETTERS[1:n_row], WellColumn = 1:n_col)
      
      for (iS in template_sheets[[iF]]) {
        tryCatch({
          df <- as.data.frame(
            readxl::read_excel(
              template_file[[iF]],
              sheet = iS,
              col_names = paste0("x", 1:n_col),
              range = plate_range
            )
          )
        }, error = function(e) {
          stop(sprintf(
            "Error loading %s. Please check logs: %s",
            template_file[[iF]],
            e
          ))
        })
        df$WellRow <- LETTERS[1:n_row]
        df_melted <- reshape2::melt(df, id.vars = "WellRow")
        # check if metadata field already exist and correct capitalization if needed
        if (!(iS %in% metadata_fields)) {
          if (!is.null(metadata_fields) &&
              toupper(iS) %in% toupper(metadata_fields)) {
            oldiS <- iS
            iS <-
              metadata_fields[toupper(iS) == toupper(metadata_fields)]
            futile.logger::flog.info("%s corrected to match case with %s", oldiS, iS)
          } else {
            metadata_fields <- c(metadata_fields, iS)
          }
        }
        colnames(df_melted)[3] <- iS
        colnames(df_melted)[colnames(df_melted) == "variable"] <-
          "WellColumn"
        df_melted$WellColumn <-
          gsub("x", "", df_melted$WellColumn)
        df_template <-
          base::merge(df_template, df_melted, by = c("WellRow", "WellColumn"))
      }
      df_template$Template <- template_filename[iF]
      colnames(df_template) <-
        check_metadata_names(colnames(df_template),
                             df_name = template_filename[iF])
      all_templates <- as.data.frame(data.table::rbindlist(list(all_templates, df_template), fill = TRUE))
      
    }
    futile.logger::flog.info("Templates loaded successfully!")
    return(all_templates)
  }


#' Load results from tsv
#'
#' This functions loads and checks the results file(s)
#'
#' @param results_file character, file path(s) to template(s)
#'
load_results_tsv <-
  function(results_file) {
    # results_file is a string or a vector of strings
    results_filename <- basename(results_file)
    
    # read all files
    all_results <- data.frame()
    for (iF in 1:length(results_file)) {
      futile.logger::flog.info("Reading file", results_file[iF])
      tryCatch({
        df <-
        utils::read.table(results_file[iF], sep = "\t", header = TRUE, na.strings=c("", "NA")) %>%
          stats::na.omit()
      }, error = function(e) {
        stop(sprintf("Error reading %s", results_file[[iF]]))
      })
      # skip_empty_rows flag needs to be TRUE even if it ends up not skipping empty rows
      if (dim(df)[2] == 1) {
        tryCatch({
          # likely a csv file
          df <-
            utils::read.csv(results_file[iF], header = TRUE, na.strings=c("", "NA")) %>%
            stats::na.omit()
        }, error = function(e) {
          stop(sprintf("Error reading %s", results_file[[iF]]))
        })
      }
      
      for (coln in c("Barcode",
                     gDRutils::get_identifier("well_position"),
                     "ReadoutValue")) {
        if (!(coln %in% colnames(df))) {
          futile.logger::flog.error("%s needs to be a column of %s", coln, results_filename[iF])
        }
      }
      if (dim(unique(df[, c("Barcode", gDRutils::get_identifier("well_position"))]))[1] !=
          dim(df[, c("Barcode", gDRutils::get_identifier("well_position"))])[1]) {
        futile.logger::flog.error("Multiple rows with the same Barcode and Well in %s",
                                  results_filename[iF])
      }
      if (!("BackgroundValue" %in% colnames(df)))
        df$BackgroundValue <- 0
      
      futile.logger::flog.info("File %s read; %d wells", results_filename[iF], dim(df)[1])
      all_results <- rbind(all_results, df)
      
      futile.logger::flog.info("File done")
    }
    
    if (dim(unique(df[, c("Barcode", gDRutils::get_identifier("well_position"))]))[1] !=
        dim(df[, c("Barcode", gDRutils::get_identifier("well_position"))])[1]) {
      futile.logger::flog.error("Multiple rows with the same Barcode and Well across all files")
    }
    
    return(all_results)
  }


#' Load results from xlsx
#'
#' This functions loads and checks the results file(s)
#'
#' @param results_file character, file path(s) to template(s)
load_results_EnVision <-
  function(results_file) {
    # Assertions:
    checkmate::assert_character(results_file)
    
    results_filename <- basename(results_file)
    # results_file is a string or a vector of strings
    
    # test if the result files are .tsv or .xls(x) files
    isExcel <- sapply(results_file, function(x) {
      return(tools::file_ext(x) %in% c("xlsx", "xls"))
    })
    
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
    for (iF in 1:length(results_file)) {
      for (iS in results_sheets[[iF]]) {
        futile.logger::flog.info("Reading file %s, sheet %s", results_file[[iF]], iS)
        if (iS == 0) {
          fInfo <- read_EnVision(results_file[[iF]])
          df <- fInfo$df
          isEdited <- fInfo$isEdited
        } else {
          isEdited <- TRUE
          # expect an Excel spreadsheet
          if (length(results_sheets[[iF]]) > 1) {
            # if multiple sheets, assume 1 plate per sheet
            tryCatch({
              df <-
                readxl::read_excel(
                  results_file[[iF]],
                  sheet = iS,
                  col_names = paste0("x", 1:48),
                  range = "A1:AV32"
                )
            }, error = function(e) {
              stop(sprintf("Error reading %s, sheet %s", results_file[[iF]], iS))
            })
          } else {
            tryCatch({
              df <- readxl::read_excel(results_file[[iF]],
                                       sheet = iS,
                                       col_names = FALSE)
            }, error = function(e) {
              stop(sprintf("Error reading %s, sheet %s", results_file[[iF]], iS))
            })
            colnames(df) <-
              col_names <- paste0("x", 1:dim(df)[2])
          }
          df <-
            df[,!apply(df[1:24,], 2, function(x)
              all(is.na(x)))]
          # remove extra columns
          # limit to first 24 rows in case Protocol information is
          # exported which generate craps at the end of the file
        }
        
        # not empty rows
        # before discarding the rows; move ''Background information'' in the next row
        Bckd_info_idx <-
          which(as.data.frame(df)[, 1] %in% 'Background information')
        if (length(Bckd_info_idx) > 0) {
          df[Bckd_info_idx + 1, 1] = df[Bckd_info_idx, 1]
          df[Bckd_info_idx, 1] = ''
        }
        
        if (isEdited) {
          # need to do some heuristic to find where the data is
          full_rows <-
            !apply(df[,-6:-1], 1, function(x)
              all(is.na(x)))
          # don't consider the first columns as these may be metadata
          # if big gap, delete what is at the bottom (Protocol information)
          gaps <-
            min(which(full_rows)[(diff(which(full_rows)) > 20)] + 1, dim(df)[1])
          df <-
            df[which(full_rows)[which(full_rows) <= gaps],] # remove extra rows
          
          # get the plate size
          n_col <-
            1.5 * 2 ** ceiling(log2((dim(df)[2] - 2) / 1.5)) # -2 ot have some buffer
          n_row <- n_col / 1.5
          
          # add empty column to complete plate (assume left column is #1)
          if (ncol(df) < n_col) {
            df[, (ncol(df) + 1):n_col] <- NA
          }
          
          # get the barcode(s) in the sheet; expected in column C (third one)
          Barcode_idx <-
            which(as.data.frame(df)[, 3] %in% "Barcode")
          # run through all plates
          for (iB in Barcode_idx) {
            # two type of format depending on where Background information is placed
            if (any(as.data.frame(df)[iB + (1:4), 1] %in% "Background information")) {
              ref_bckgrd <-
                which(as.data.frame(df)[iB + (1:4), 1] %in% "Background information")
              readout_offset <- 1 + ref_bckgrd
              stopifnot(as.character(df[iB + ref_bckgrd, 4]) %in% 'Signal')
              BackgroundValue <-
                as.numeric(df[iB + ref_bckgrd + 1, 4])
            } else {
              # export without background information
              # case of " Exported with EnVision Workstation version 1.13.3009.1409 "
              readout_offset <- 1
              BackgroundValue <- 0
            }
            
            # check the structure of file is ok
            check_values <-
              as.matrix(df[iB + readout_offset + c(0, 1, n_row, n_row + iB + readout_offset), n_col])
            
            Barcode <- as.character(df[iB + 1, 3])
            if (any(c(is.na(check_values[2:3]),!is.na(check_values[c(1, 4)])))) {
              stop(
                sprintf(
                  "In result file %s (sheet %s) readout values are misplaced for plate %s",
                  results_filename[[iF]],
                  iS,
                  as.character(df[iB + 1, 3])
                )
              )
            }
            
            readout <-
              as.matrix(df[iB + readout_offset + (1:n_row), 1:n_col])
            
            # check that the plate size is consistent and contains values
            if (any(is.na(readout))) {
              stop(
                sprintf(
                  "In result file %s (sheet %s) readout values are misplaced for plate %s",
                  results_filename[[iF]],
                  iS,
                  as.character(df[iB + 1, 3])
                )
              )
            }
            
            df_results <- data.frame(
              Barcode = Barcode,
              WellRow = LETTERS[1:n_row],
              WellColumn = as.vector(t(matrix(
                1:n_col, n_col, n_row
              ))),
              ReadoutValue = as.numeric(as.vector(readout)),
              BackgroundValue = BackgroundValue
            )
            futile.logger::flog.info("Plate %s read; %d wells",
                                     as.character(df[iB + 1, 3]),
                                     dim(df_results)[1])
            all_results <- rbind(all_results, df_results)
	  }
          } else {
            # proper original EnVision file
            n_row = fInfo$n_row
            n_col = fInfo$n_col
            Barcode = df[3, 3]
            readout <-
              as.matrix(df[4 + (1:n_row), 1:n_col])
            
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
            WellRow = LETTERS[1:n_row],
            WellColumn = as.vector(t(matrix(
              1:n_col, n_col, n_row
            ))),
            ReadoutValue = as.numeric(as.vector(readout)),
            BackgroundValue = BackgroundValue
          )
          print(paste(
            "Plate",
            as.character(Barcode),
            "read;",
            dim(df_results)[1],
            "wells"
          ))
          all_results <- rbind(all_results, df_results)
        }
        
      }
      futile.logger::flog.info("File done")
    }
    return(all_results)
  }


#' check_metadata_names
#'
#' Check whether all metadata names are correct
#'
#' @param col_df a character with colnames of df
#' @param df_name a name of dataframe ("" by default)
#' @param df_type a type of a dataframe (NULL by default)
#'
#' @return
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
    
    # first check for required column names
    if (!is.null(df_type)) {
      if (df_type == "manifest") {
        expected_headers <- gDRutils::get_header("manifest")
      } else if (df_type == "template") {
        expected_headers <- gDRutils::get_identifier("drug")
      } else if (df_type == "template_treatment") {
        expected_headers <- c(gDRutils::get_identifier("drug"), "Concentration")
      }
      
      headersOK <- toupper(expected_headers) %in% toupper(col_df)
      if (any(!headersOK)) {
        stop(
          sprintf(
            "Template does not contains all expected headers for a '%s'. '%s' is/are required. Please correct your template.",
            df_type,
            toString(expected_headers[!(expected_headers %in% col_df)])
          )
        )
      }
      if (df_type == "template_treatment") {
        # assess if multiple drugs and proper pairing
        n_drug <-
          agrep(gDRutils::get_identifier("drug"), col_df, ignore.case = TRUE)
        n_conc <-
          agrep("Concentration", col_df, ignore.case = TRUE)
        if (length(n_drug) != length(n_conc)) {
          stop(
            sprintf(
              "Template file(s) %s do/does not contain the same number of Gnumber_* and Concentration_* sheets. Gnumber_* and Concentration_* sheets are required.
              Please correct your template.",
              df_name
            )
          )
        }
        if (length(n_drug) > 1) {
          trt_sheets <- c(
            paste0(gDRutils::get_identifier("drug"), "_",
                   2:length(n_drug)),
            paste0("Concentration_", 2:length(n_conc))
          )
          if (!(all(toupper(trt_sheets) %in% toupper(col_df)))) {
            stop(sprintf(
              "Template file %s does not contain %s sheets. Please correct your template.",
              df_name,
              toString(trt_sheets[!(toupper(trt_sheets) %in% toupper(col_df))])
            ))
          }
        }
      }
    }
    check_headers <-
      setdiff(gDRutils::get_header("reserved"), gDRutils::get_identifier("well_position"))
    
    
    corrected_names <- col_df
    
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
    
    # check for wrong metadata field names (including dash, starting with number, ... )
    bad_names <-
      regexpr("\\W", corrected_names) > 0 |
      regexpr("\\d", corrected_names) == 1
    if (any(bad_names)) {
      stop(
        sprintf(
          "Metadata field names for %s cannot contain special characters or start with a number: ",
          df_name,
          toString(corrected_names[bad_names])
        )
      )
    }
    
    # common headers that are written in a specific way
    # throw warning if close match and correct upper/lower case for consistency
    for (i in 1:length(gDRutils::get_header("controlled"))) {
      case_match <- setdiff(
        grep(paste0(gDRutils::get_header("controlled")[i], "$"), corrected_names, ignore.case = TRUE),
        grep(paste0(gDRutils::get_header("controlled")[i], "$"), corrected_names)
      )
      if (length(case_match) > 0) {
        corrected_names[case_match] <- gDRutils::get_header("controlled")[i]
        futile.logger::flog.warn("Header %s in %s corrected to %s",
                                 corrected_names[case_match],
                                 df_name,
                                 gDRutils::get_header("controlled")[i])
      }
    }
    
    # check for headers that are reserved for downstream analyses
    if (any(corrected_names %in% check_headers)) {
      stop(sprintf(
        "Metadata field name: %s in %s is not valid (reserved for output)",
        toString(intersect(check_headers, corrected_names)),
        df_name
      ))
    }
    
    return(corrected_names)
  }


#' Read envision files
#'
#' This function reads file from the EnVision Workstation
#'
#' @param file 
#' @param nrows 
#' @param seps 
#'
#' @return
#' @export
#'
read_EnVision = function(file,
                         nrows = 10000,
                         seps = c(',', '\t')) {
  # Assertions:
  checkmate::assert_string(file)
  checkmate::assert_number(nrows)
  checkmate::assert_character(seps)
  con <- file(file)
  open(con)
  
  results.list <- list()
  
  current.line <- 1
  while (length(line <-
                readLines(con, n = 1, warn = FALSE)) > 0 & current.line < nrows) {
    results.list[[current.line]] <- line
    current.line <- current.line + 1
  }
  close(con)
  
  n_sep <- colSums(vapply(seps, function(sep) {
    vapply(results.list[1:10], function(line)
      length(unlist(strsplit(line, split = sep))),
      integer(length = 1))
  }, integer(length = 10)))
  sep <- n_sep [n_sep == max(n_sep)]
  if (sep < 30) {
    stop(sprintf("Can't guess separator of the delimited file: %s", file))
  }
  sep <- names(sep)
  
  results.list <-
    lapply(results.list, function(line)
      unlist(strsplit(line, split = sep)))
  
  # identify if original csv file or not
  if (which(vapply(results.list, function(x)
    grepl("Plate information", x[1]), logical(1))) != 1) {
    # fails if not the right header
    stop(sprintf(
      "Error reading %s: not an original EnVision .csv file",
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
    n_col <- 1.5 * 2 ** ceiling(log2((n_col - 1) / 1.5))
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
      stop(sprintf("Error reading %s: wrong plate size", results_file[[iF]]))
    }
    n_row <- n_col / 1.5
  } else {
    n_col <- max(vapply(results.list[5:10], length, integer(length = 1)))
    n_row <- sum(vapply(results.list[5:which(vapply(results.list, function(x)
      grepl("Basic assay information", x[1]), logical(1)))],
      length, integer(1)) == n_col)
    if (log2(1.5 * n_row / n_col) != 0)  {
      stop(sprintf("Error reading %s: wrong plate size", results_file[[iF]]))
    }
  }
  
  # pad the lines with NA if not full before creating the dataframe
  results.list <- lapply(results.list, function(x) {
    if (length(x) < n_col) {
      x <- c(x, array(NA, n_col - length(x)))
    } else if (length(x) > n_col) {
      x <- x[1:n_col]
    }
    x[x == '' & !is.na(x)] <- NA
    return(x)
  })
  df_ <- as.data.frame(do.call(rbind, results.list), stringsAsFactors = F)
  colnames(df_) <- paste0('x', 1:n_col)
  
  rownames(df_) <- NULL
  return(list(
    df = df_,
    n_col = n_col,
    n_row = n_row,
    isEdited = isEdited
  ))
}

