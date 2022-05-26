#' Parse D300
#'
#' This function parses a D300 *.tdd file (XML format) into a data.frame
#'
#' @param D300_file string, file path to D300 .tdd file
#
#' @export
#'
parse_D300 <- 
  function(D300_file) {
    
    assertthat::assert_that(assertthat::is.readable(D300_file), msg = "'D300_file' must be a readable path")
    
    #open D300 XML format
    D300_xml.tree <- XML::xmlTreeParse(D300_file, useInternal = TRUE) 
    top <- XML::xmlRoot(D300_xml.tree)
    #check concentrations are matching 
    conc_unit <- XML::xmlValue(top[["ConcentrationUnit"]])
    mol_conc_unit <- XML::xmlValue(top[["MolarityConcentrationUnit"]])
    assertthat::assert_that(conc_unit == mol_conc_unit, 
                            msg = "Mismatch between the units for ConcentrationUnit and MolarityConcentrationUnit")
    
    #extract information for every fluid (i.e. drugs)
    fluids <- XML::xpathSApply(top, ".//Fluids", XML::xmlChildren)
    nfluids <- length(fluids)
    drug_cols <- c("ID", "Name", "Stock_Conc", "Stock_Unit")
    df_drug <- data.frame(matrix(ncol = length(drug_cols), nrow = 0))
    colnames(df_drug)<-drug_cols 
    for (fi in 1:nfluids) {
      id <- XML::xmlAttrs(top[["Fluids"]][fi][["Fluid"]])
      name <- XML::xmlValue(top[["Fluids"]][fi][["Fluid"]][["Name"]])
      stock_conc <- XML::xmlValue(top[["Fluids"]][fi][["Fluid"]][["Concentration"]])
      conc_unit <- XML::xmlValue(top[["Fluids"]][fi][["Fluid"]][["ConcentrationUnit"]])
      df_drug_entry <- data.frame(t(c(id, name, stock_conc, conc_unit)))
      colnames(df_drug_entry) <- drug_cols
      df_drug <- rbind(df_drug, df_drug_entry)
    }
    #convert unit volumes to uL 
    vol_unit  <- XML::xmlValue(top[["VolumeUnit"]])
    if (vol_unit == "nL") {
      vol_conversion <- 1e-3
      vol_unit_conv <- "µL"
    }else if (volumeUnit == "µL") {
      vol_conversion <- 1
      vol_unit_conv <- "µL"
    }else if (volumeUnit == "mL") {
      vol_conversion <- 1e3
      vol_unit_conv <- "µL"
    }else{
      stop(sprintf("Error with %s unit in VolumeUnit, not supported.",vol_unit))
    }
    # ff there is DMSO backfill defined throw a warning, support not yet implemented
    backfills <- XML::xpathSApply(top, ".//Backfills/Backfill", XML::xmlChildren)
    if (length(backfills)>0) {
      warning("Backfill identified in D300 but not supported.")
    }
    #initialize data.frame for treatments
    trt_cols <- c("D300_Plate_N", "D300_Barcode", "Dimension", "Row", "Col", 
                  "Volume","Volume_Unit", "ID", "Concentration", "Unit")
    df_trt <- data.frame(matrix(ncol = length(trt_cols), nrow = 0))
    colnames(df_trt)<-trt_cols 
    #extract drug dispensing information for each plate 
    plates <- XML::xpathSApply(top, ".//Plates", XML::xmlChildren)
    nplates <- length(plates)
    for (pi in 1:nplates) {
      #plate info
      rows_plate <- XML::xmlValue(top[["Plates"]][pi][["Plate"]][["Rows"]])
      cols_plate <- XML::xmlValue(top[["Plates"]][pi][["Plate"]][["Cols"]])
      plate_dim <- paste("(", rows_plate, ",", cols_plate,")", sep = "")
      assay_vol <- as.double(XML::xmlValue(top[["Plates"]][pi][["Plate"]][["AssayVolume"]]))
      assay_vol_conv <- as.double(assay_vol) * vol_conversion
      barcode_plate <- XML::xmlValue(top[["Plates"]][pi][["Plate"]][["Name"]])
      #this check if the plate is randomize should probably be changed 
      randomize <- XML::xmlValue(top[["Plates"]][pi][["Plate"]][["Randomize"]])
      if (randomize != "") {
        warning("Randomization of D300 plate possibly detected, but not supported yet.")
      }
      
      #extract drug dispensing information for each well 
      wells <- XML::xpathSApply(top[["Plates"]][pi][["Plate"]], ".//Wells", XML::xmlChildren)
      nwells <- length(wells)
      for (wi in 1:nwells) {
        #indexes of row and col start from zero, so add one
        well_attr <-XML::xmlAttrs(top[["Plates"]][pi][["Plate"]][["Wells"]][wi][["Well"]])
        row_well <- strtoi(well_attr[["Row"]]) +1
        col_well <- strtoi(well_attr[["Col"]]) +1
        
        #extract information each fluid delivered in well 
        fluids <- XML::xpathSApply(top[["Plates"]][pi][["Plate"]][["Wells"]][wi][["Well"]], 
                                   ".//Fluid", XML::xmlChildren)
        nfluids <- length(fluids)
        for (fi in 1:nfluids) {
          id_fluid <- XML::xmlAttrs(top[["Plates"]][pi][["Plate"]][["Wells"]][wi][["Well"]][[fi]])
          conc_fluid <- XML::xmlValue(top[["Plates"]][pi][["Plate"]][["Wells"]][wi][["Well"]][[fi]])
          #define single entry
          df_trt_entry <- data.frame(t(c(pi, 
                                         barcode_plate, 
                                         plate_dim, 
                                         row_well,
                                         col_well,
                                         assay_vol_conv,
                                         vol_unit_conv,
                                         id_fluid,
                                         conc_fluid,
                                         conc_unit)))
          colnames(df_trt_entry) <- trt_cols
          df_trt <- rbind(df_trt, df_trt_entry)
        }
      }
    }
    
    #merge to return a unique dataset
    df_D300 <- merge(df_trt, df_drug, by.x = "ID", by.y = "ID", all.x = TRUE)
    return(df_D300)
  }

#' Import D300
#'
#' This functions takes a D300 file and generates corresponding template files
#'
#' @param D300_file character, file path to D300 file
#' @param Gnum_file character, file path to file with mapping from D300 names to Gnumbers 
#' @param destination_path character, path to folder where template 
#' files will be generated
#' @param instrument character
#'
#' @export
#'
import_D300 <-
  function(D300_file, Gnum_file, destination_path) {
    
    assertthat::assert_that(is.character(D300_file), msg = "'D300_file' must be a character vector")
    assertthat::assert_that(is.character(Gnum_file), msg = "'Gnum_file' must be a character vector")
    assertthat::assert_that(is.character(destination_path), msg = "'destination_path' must be a character vector")
    assertthat::assert_that(assertthat::is.readable(D300_file), msg = "'D300_file' must be a readable path")
    assertthat::assert_that(assertthat::is.readable(Gnum_file), msg = "'Gnum_file' must be a readable path")
    assertthat::assert_that(assertthat::is.readable(destination_path), msg = "'destination_path' must be a readable path")
    
    #load a file that maps D300 labels to Gnumbers
    D300_Gnumber_sheets <- readxl::excel_sheets(Gnum_file)
    #check there is just one sheet
    if (length(D300_Gnumber_sheets) != 1) {
      futile.logger::flog.error("No or too many data sheets found in: %s",
                                D300_Gnum_file)
    }
    #load mapping
    df_D300_Gnum <- readxl::read_excel(Gnum_file,
                                       sheet = D300_Gnumber_sheets[[1]],
                                       col_names = TRUE)
    #parse D300 file 
    treatment <- parse_D300(D300_file)
    #map names to Gnumber
    treatment <- merge(treatment, df_D300_Gnum, by.x = "Name", by.y = "D300_Label", all.x = TRUE)
    #if barcode is missing, use ID instead
    idx <- is.na(treatment$D300_Barcode)
    treatment[idx, c("D300_Barcode")] <- treatment[idx, c("D300_Plate_N")]
    #extract unique barcodes
    uid <- unique(treatment$D300_Plate_N)
    #create a treatment file for each plates
    for (i in 1:length(uid)) {
      
      #filter only that plate
      idx <- (treatment$D300_Plate_N == uid[i])
      trt_filt <- treatment[idx, ]
      #create a list with Gnumber and Concentration 
      trt_filt$Gnumber_Concentration <- apply(trt_filt, 1, function(x) list(x["Gnumber"], x["Concentration"]))
      trt_gnumber_conc <- reshape2::dcast(trt_filt, Row ~ Col, 
                                          value.var = c("Gnumber_Concentration"), 
                                          fun.aggregate = list)
      rownames(trt_gnumber_conc) <- trt_gnumber_conc$Row
      trt_gnumber_conc <- dplyr::select(trt_gnumber_conc, -c("Row"))
      #count number of drugs,conc in each well 
      trt_n_drugs <- apply(trt_gnumber_conc, c(1, 2), function(x) length(x[[1]]))
      max_drugs <- max(trt_n_drugs)
      col_idx <- strtoi(colnames(trt_gnumber_conc))
      row_idx <- strtoi(rownames(trt_gnumber_conc))
      max_row_idx <- max(row_idx) 
      max_col_idx <- max(col_idx)
      
      #for each drug create a Gnumber and Concentration information for each well
      for (j in 1:max_drugs) {
        #create empty text matrix
        gnum_txt <- rep(c(""), each = max_row_idx * max_col_idx)
        dim(gnum_txt) <- c(max_row_idx, max_col_idx)     
        conc_txt <- rep(c(""), each = max_row_idx * max_col_idx)
        dim(conc_txt) <- c(max_row_idx, max_col_idx)     
        #for each row and column
        for (ri in 1:length(row_idx)) {
          for (ci in 1:length(col_idx)) {
            #extract the drugs in that row, col 
            drug_entry <- trt_gnumber_conc[[ri, ci]]
            if (length(drug_entry) >= j) {
              gnum_txt_now <- trt_gnumber_conc[[ri, ci]][[j]][[1]]
              conc_txt_now <- trt_gnumber_conc[[ri, ci]][[j]][[2]]
              if (gnum_txt_now %in% c("vehicle", "untreated")) {
                #replace concentration to zero if that drugs is associated to vehicle or untreated (e.g. DMSO)
                gnum_txt[row_idx[ri], col_idx[ci]] <- gnum_txt_now
                conc_txt[row_idx[ri], col_idx[ci]] <- 0.0
              } else {
                gnum_txt[row_idx[ri], col_idx[ci]] <- trt_gnumber_conc[[ri, ci]][[j]][[1]]
                conc_txt[row_idx[ri], col_idx[ci]] <- trt_gnumber_conc[[ri, ci]][[j]][[2]]
              }
            } else {
              #if there is no 2nd, 3rd etc.. drug specified, set the corresponding entry to untreated
              gnum_txt[row_idx[ri], col_idx[ci]] <- "untreated"
              conc_txt[row_idx[ri], col_idx[ci]] <- 0.0
            }
          }
        }
        
        #create data.frame Gnumber and Concentrations to crate excell files
        gnumber_sheet <- data.frame(gnum_txt)
        conc_sheet <- data.frame(conc_txt)
        
        #create sheet name 
        if (j == 1) {
          gnum_sname <- "Gnumber"
          conc_sname <- "Concentration"
        }else{
          gnum_sname <- sprintf("Gnumber_%d", j)
          conc_sname <- sprintf("Concentration_%d", j)
        }
        
        #write sheets in excel file 
        treat_file <- sprintf("trt_P%s.xlsx", uid[i])
        xlsx::write.xlsx(gnumber_sheet, 
                         file.path(destination_path, treat_file), 
                         gnum_sname,
                         col.names = FALSE, 
                         row.names = FALSE,
                         append = TRUE) 
        xlsx::write.xlsx(conc_sheet, 
                         file.path(destination_path, treat_file), 
                         conc_sname, 
                         col.names = FALSE, 
                         row.names = FALSE,
                         append = TRUE) 
      }
    }
  }

