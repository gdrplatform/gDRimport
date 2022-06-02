#' Import D300
#'
#' This functions takes a D300 file and generates corresponding template files
#'
#' @param D300_file character, file path to D300 file
#' @param metadata_file character, file path to file with mapping from D300 names to Gnumbers 
#' @param destination_path character, path to folder where template 
#' files will be generated
#'
#' @export
#'
import_D300 <-
  function(D300_file, metadata_file, destination_path) {
    # Assertions.
    assertthat::assert_that(is.character(destination_path), msg = "'destination_path' must be a character vector")
    assertthat::assert_that(assertthat::is.readable(destination_path), 
                            msg = "'destination_path' must be a readable path")
    
    Gnums <- parse_D300_metadata_file(metadata_file)
    D300 <- parse_D300_xml(D300_file)
    treatment <- merge_D300_w_metadata(D300, Gnums)

    treatment <- fill_NA(treatment, from = "D300_Barcode", with = "D300_Plate_N")

    uplates <- unique(treatment$D300_Plate_N)

    untreated_tags <- gDRutils::get_env_identifiers("untreated_tag")
    #create a treatment file for each plates
    for (i in seq_along(uplates)) {
      wb <- openxlsx::createWorkbook()
      
      #filter only that plate
      idx <- (treatment$D300_Plate_N == uplates[i])
      trt_filt <- treatment[idx, ]

      #create a list with Gnumber and Concentration 
      trt_filt$Gnumber_Concentration <- apply(trt_filt, 1, function(x) list(x["Gnumber"], x["Concentration"]))
      trt_gnumber_conc <- reshape2::dcast(trt_filt, Row ~ Col, 
                                          value.var = c("Gnumber_Concentration"), 
                                          fun.aggregate = list)
      rownames(trt_gnumber_conc) <- trt_gnumber_conc$Row
      trt_gnumber_conc <- trt_gnumber_conc[, setdiff(colnames(trt_gnumber_conc), "Row")]

      #count number of drugs,conc in each well 
      trt_n_drugs <- apply(trt_gnumber_conc, c(1, 2), function(x) length(x[[1]]))
      max_drugs <- max(trt_n_drugs)

      col_idx <- strtoi(colnames(trt_gnumber_conc))
      row_idx <- strtoi(rownames(trt_gnumber_conc))
      nrow <- max(row_idx) 
      ncol <- max(col_idx)
      nwells <- nrow * ncol
      
      #for each drug create a Gnumber and Concentration information for each well
      for (j in seq_len(max_drugs)) {
        conc_mat <- drug_mat <- matrix(rep("", nwells), nrow = nrow, ncol = ncol)

        for (m in seq_along(row_idx)) {
          for (n in seq_along(col_idx)) {

            drug_entry <- trt_drugber_conc[[m, n]]
            if (length(drug_entry) >= j) {
              drug <- drug_entry[[j]][[1]]
              conc <- drug_entry[[j]][[2]]

              # Vehicle or untreated drugs assigned concentration zero.
              if (drug %in% untreated_tags) {
                conc <- 0.0
              }

            } else {
              #if there is no 2nd, 3rd etc.. drug specified, set the corresponding entry to untreated
              drug <- untreated_tags[[1]]
              conc <- 0.0
            }

            conc_mat[row_idx[m], col_idx[n]] <- conc
            drug_mat[row_idx[m], col_idx[n]] <- drug
          }
        }
        
        drug_data <- data.frame(drug_mat)
        conc_data <- data.frame(conc_mat)
        
        drug_sname <- "Gnumber"
        conc_sname <- "Concentration"
        if (j != 1L) {
          drug_sname <- paste0(drug_sname, "_", j)
          conc_sname <- paste0(conc_sname, "_", j)
        }
        
        openxlsx::addWorksheet(wb, drug_sname)
        openxlsx::writeData(wb, sheet = (j * 2) - 1, drug_data, colNames = FALSE)
        openxlsx::addWorksheet(wb, conc_sname)
        openxlsx::writeData(wb, sheet = (j * 2), conc_data, colNames = FALSE)
      }
      
      fname <- sprintf("trt_P%s.xlsx", uplates[i])
      openxlsx::saveWorkbook(wb, file.path(destination_path, fname), overwrite = TRUE)
    }
  }


merge_D300_w_metadata <- function(D300, Gnums) {
  validate_columns <- function(col, df) {
    if (!col %in% colnames(df)) {
      stop(sprintf("missing required column: '%s' in '%s'", col, quote(df)))
    }
    invisible(NULL)
  }

  merge_trt_col <- "Name"
  validate_columns(merge_trt_col, D300)

  merge_metadata_col <- "D300_Label"
  validate_columns(merge_metadata_col, Gnums)

  merge(D300, Gnums, by.x = merge_trt_col, by.y = merge_metadata_col, all.x = TRUE)
}

#########
# D300
#########

#' Parse D300
#'
#' This function parses a D300 *.tdd file (XML format) into a data.frame
#'
#' @param D300_file string, file path to D300 .tdd file
#'
#' @return data.frame representing input \code{D300_file}.
#
#' @export
#'
parse_D300_xml <- function(D300_file) {
    assertthat::assert_that(is.character(D300_file), msg = "'D300_file' must be a character vector")
    assertthat::assert_that(assertthat::is.readable(D300_file), msg = "'D300_file' must be a readable path")
    
    # Open D300 XML format.
    D300_xml.tree <- XML::xmlTreeParse(D300_file, useInternal = TRUE) 
    top <- XML::xmlRoot(D300_xml.tree)

    # Retrieve units.
    vol_unit  <- XML::xmlValue(top[["VolumeUnit"]])
    conc_unit <- XML::xmlValue(top[["ConcentrationUnit"]])
    mol_conc_unit <- XML::xmlValue(top[["MolarityConcentrationUnit"]])

    # Assertions.
    assertthat::assert_that(conc_unit == mol_conc_unit, 
                            msg = "Mismatch between the units for ConcentrationUnit and MolarityConcentrationUnit")

    # if there is DMSO backfill defined throw a warning, support not yet implemented
    backfills <- XML::xpathSApply(top, ".//Backfills/Backfill", XML::xmlChildren)
    if (length(backfills) > 0) {
      warning("Backfill identified in D300 but not supported.")
    }
    
    id_col <- "ID"
    df_drug <- get_D300_xml_drugs(top, id_col)
    df_trt <- get_D300_xml_treatments(top, id_col, vol_unit, conc_unit)
    
    df_D300 <- merge(df_trt, df_drug, by.x = id_col, by.y = id_col, all.x = TRUE)
    df_D300
  }


get_D300_xml_drugs <-
  function(xml_tree_root, id_col = "ID") {

    drug_cols <- c(id_col, "Name", "Stock_Conc", "Stock_Unit")

    #extract information for every fluid (i.e. drugs)
    fluids <- XML::xpathSApply(xml_tree_root, ".//Fluids", XML::xmlChildren)
    nfluids <- length(fluids)
    df_drug <- vector("list", nfluids)
    for (fi in seq_len(nfluids)) {
      fluid <- xml_tree_root[["Fluids"]][fi][["Fluid"]]
      id <- XML::xmlAttrs(fluid)
      name <- XML::xmlValue(fluid[["Name"]])
      stock_conc <- XML::xmlValue(fluid[["Concentration"]])
      conc_unit <- XML::xmlValue(fluid[["ConcentrationUnit"]])
      df_drug[[fi]] <- data.frame(t(c(id, 
                                     name, 
                                     stock_conc, 
                                     conc_unit)))
      colnames(df_drug[[fi]]) <- drug_cols
    }
    do.call("rbind", df_drug)
  }    


get_D300_xml_treatments <-
  function(xml_tree_root, id_col = "ID", vol_unit, conc_unit) {

    #initialize data.frame for treatments
    trt_cols <- c("D300_Plate_N", "D300_Barcode", "Dimension", "Row", "Col", 
                  "Volume", "Volume_Unit", id_col, "Concentration", "Unit")
    df_trt <- data.frame(matrix(ncol = length(trt_cols), nrow = 0))
    colnames(df_trt) <- trt_cols 

    #extract drug dispensing information for each plate 
    plates <- XML::xpathSApply(xml_tree_root, ".//Plates", XML::xmlChildren)
    nplates <- length(plates)
    for (pli in seq_len(nplates)) {
      plate <- xml_tree_root[["Plates"]][pli][["Plate"]]

      #plate info
      rows_plate <- XML::xmlValue(plate[["Rows"]])
      cols_plate <- XML::xmlValue(plate[["Cols"]])
      plate_dim <- sprintf("(%s,%s)", rows_plate, cols_plate)
      assay_vol <- XML::xmlValue(plate[["AssayVolume"]])
      desired_unit <- "µL"
      assay_vol_conv <- convert_units(assay_vol, from = vol_unit, to = desired_unit)
      barcode_plate <- XML::xmlValue(plate[["Name"]])
      # check if the plate is randomized; should probably be changed 
      randomize <- XML::xmlValue(plate[["Randomize"]])
      if (randomize != "") {
        warning("Randomization of D300 plate possibly detected, but not supported yet.")
      }
      
      #extract drug dispensing information for each well 
      wells <- XML::xpathSApply(plate, ".//Wells", XML::xmlChildren)
      nwells <- length(wells)
      for (wi in seq_len(nwells)) {
        
        well <- plate[["Wells"]][wi][["Well"]]
        #indexes of row and col start from zero, so add one
        well_attr <- XML::xmlAttrs(well)
        row_well <- strtoi(well_attr[["Row"]]) + 1
        col_well <- strtoi(well_attr[["Col"]]) + 1
        
        #extract information each fluid delivered in well 
        fluids <- XML::xpathSApply(well, ".//Fluid", XML::xmlChildren)
        nfluids <- length(fluids)
        for (fi in seq_len(nfluids)) {
          id_fluid <- XML::xmlAttrs(well[[fi]])
          conc_fluid <- XML::xmlValue(well[[fi]])
          
          #define single entry
          df_trt_entry <- data.frame(t(c(pli, 
                                         barcode_plate, 
                                         plate_dim, 
                                         row_well,
                                         col_well,
                                         assay_vol_conv,
                                         desired_unit,
                                         id_fluid,
                                         conc_fluid,
                                         conc_unit)))
          colnames(df_trt_entry) <- trt_cols
          df_trt <- rbind(df_trt, df_trt_entry)
        }
      }
    }
    df_trt
  }


get_conversion_factor <- function(from, to = "µL") {
  if (to != "µL") {
    stop(sprintf("conversion to unit '%s' not supported", to))
  }

  # nolint start
  switch(from,
    "nL" = 1e-3,
    "µL" = 1,
    "mL" = 1e3,
    stop(sprintf("unsupported conversion factor: '%s'", from))
  )
  # nolint end
}


convert_units <- function(x, from, to) {
  conversion_factor <- get_conversion_factor(from, to) 
  as.double(x) * conversion_factor
}

#########
# Gnum
#########

parse_D300_metadata_file <- function(metadata_file) {
  D300_Gnum_sheets <- readxl::excel_sheets(metadata_file)
  nsheets <- length(D300_Gnum_sheets)

  # Assertions.
  assertthat::assert_that(is.character(metadata_file), msg = "'metadata_file' must be a character vector")
  assertthat::assert_that(assertthat::is.readable(metadata_file), msg = "'metadata_file' must be a readable path")

  if (nsheets != 1L) {
    futile.logger::flog.error("only one data sheet is supported, found '%s' sheets in '%s'",
                              nsheets, metadata_file)
  }

  metadata <- readxl::read_excel(metadata_file,
                                     sheet = D300_Gnum_sheets[[1]],
                                     col_names = TRUE)
  metadata
}


#########
# Utils
#########

fill_NA <- function(x, from, with) {
  idx <- is.na(x[[from]])
  x[idx, from] <- x[idx, with]
  x
}
