#' Import D300
#'
#' This functions takes a D300 file and generates corresponding template files
#'
#' @param D300_file character, file path to D300 file
#' @param destination_path character, path to folder where template 
#' files will be generated
#' @param metadata_file character, file path to file with mapping from D300 names to Gnumbers. Defaults to NULL.
#' @keywords D300
#' 
#' @examples
#' td3 <- get_test_D300_data()[["f_96w"]]
#' o_path <- file.path(tempdir(), "td3")
#' dir.create(o_path)
#' import_D300(td3$d300, o_path, td3$Gnum)
#' list.files(o_path)
#' unlink(o_path, recursive = TRUE)
#'
#' @return
#' Create one Excel file per plate. Each sheet in each plate file describes 
#' the drugs and corrresponding concentrations of what was tested in each well.
#' @details
#' For example, wells treated with 2 drugs in combination will result in 4 sheets per plate.
#' \itemize{
#'  \item{Sheet 1: Drug 1}
#'  \item{Sheet 2: Conc of Drug 1}
#'  \item{Sheet 3: Drug 2}
#'  \item{Sheet 4: Conc of Drug 2}
#' }
#'
#' @export
#'
import_D300 <-
  function(D300_file, destination_path, metadata_file = NULL) {
    assertthat::assert_that(is.character(destination_path), msg = "'destination_path' must be a character vector")
    assertthat::assert_that(assertthat::is.readable(destination_path), 
                            msg = "'destination_path' must be a readable path")
    
    # Parse the D300 file first
    D300 <- parse_D300_xml(D300_file)
    D300 <- fill_NA(D300, from = "D300_Barcode", with = "D300_Plate_N")
    
    idfs <- list(
      untreated_tags = gDRutils::get_env_identifiers("untreated_tag"),
      drug_identifier = gDRutils::get_env_identifiers("drug"),
      conc_identifier = gDRutils::get_env_identifiers("concentration")) #standard identifiers
    
    # Conditionally process metadata if provided
    if (!is.null(metadata_file)) {
      Gnums <- parse_D300_metadata_file(metadata_file)
      treatment <- merge_D300_w_metadata(D300, Gnums)
    } else {
      # Use the original identifiers straight from the D300 file
      treatment <- D300
      treatment[[idfs$drug_identifier]] <- treatment$Name
    }
    
    req_cols <- c("Row", "Col")
    if (!all(present <- req_cols %in% colnames(treatment))) {
      stop(sprintf("missing required columns from D300 file: '%s'", paste0(req_cols[!present], collapse = ", ")))
    }
    uplates <- unique(treatment$D300_Plate_N)
    
    existing_files <- list.files(destination_path, pattern = "^trt_P\\d+\\.xlsx$")
    
    # Calculate the starting offset
    max_idx <- 0
    if (length(existing_files) > 0) {
      nums <- as.numeric(gsub("trt_P|\\.xlsx", "", existing_files))
      max_idx <- max(nums, na.rm = TRUE)
    }
    
    for (i in seq_along(uplates)) {
      wb <- openxlsx::createWorkbook()
      idx <- treatment$D300_Plate_N == uplates[i] # Filter to 1 plate.
      trt_filt <- treatment[idx, ]
      
      # create a list with Gnumber and Concentration 
      trt_filt$gn_conc <- apply(trt_filt, 1, function(x) list(x[idfs$drug_identifier], x[idfs$conc_identifier]))
      trt_gnumber_conc <- data.table::dcast(trt_filt, Row ~ Col, 
                                            value.var = c("gn_conc"), 
                                            fun.aggregate = list)
      rownames_trt_gnumber_conc <- trt_gnumber_conc$Row
      trt_gnumber_conc <- trt_gnumber_conc[, setdiff(colnames(trt_gnumber_conc), "Row"), with = FALSE]
      
      # count number of drugs,conc in each well 
      trt_n_drugs <- apply(trt_gnumber_conc, c(1, 2), function(x) length(x[[1]]))
      
      # Extract actual plate dimensions from the XML Dimension tag (e.g. "(8,12)")
      dim_str <- trt_filt$Dimension[1]
      dims <- as.integer(strsplit(gsub("\\(|\\)", "", dim_str), ",")[[1]])
      
      trt_info <- list(
        max_drugs_per_well =  max(trt_n_drugs),
        col_idx = strtoi(colnames(trt_gnumber_conc)),
        row_idx = strtoi(rownames_trt_gnumber_conc),
        plate_nrow = dims[1],
        plate_ncol = dims[2]
      )
      save_drug_info_per_well(trt_info, trt_gnumber_conc, wb, idfs) 
      current_file_num <- max_idx + i
      fname <- sprintf("trt_P%d.xlsx", current_file_num)
      
      openxlsx::saveWorkbook(wb, file.path(destination_path, fname), overwrite = TRUE)
    }
  }


#' for each drug create a Gnumber and Concentration information for each well
#' 
#' @param trt_info list with treatment info
#' @param trt_gnumber_conc list with treatment data
#' @param wb pointer to xlsx workbook
#' @param idfs charvec with identifiers
#' @keywords D300
#' 
#' @return \code{NULL} invisibly.
#' 
save_drug_info_per_well <-
  function(trt_info, trt_gnumber_conc, wb, idfs) {
    
    # Use the physical plate dimensions to ensure empty edge wells are populated
    nrow <- trt_info$plate_nrow
    ncol <- trt_info$plate_ncol
    
    # Fallback just in case dimension parsing failed
    if (is.null(nrow) || is.na(nrow)) nrow <- max(trt_info$row_idx)
    if (is.null(ncol) || is.na(ncol)) ncol <- max(trt_info$col_idx)
    
    nwells <- nrow * ncol
    
    for (j in seq_len(trt_info$max_drugs_per_well)) {
      
      drug_sname <- idfs$drug_identifier
      conc_sname <- idfs$conc_identifier
      if (j != 1L) {
        drug_sname <- paste0(drug_sname, "_", j)
        conc_sname <- paste0(conc_sname, "_", j)
      }
      
      # Pre-fill matrices with vehicle values (untreated + 0.0 conc)
      conc_mat <- matrix(rep(0.0, nwells), nrow = nrow, ncol = ncol)
      drug_mat <- matrix(rep(idfs$untreated_tags[[1]], nwells), nrow = nrow, ncol = ncol)
      
      for (m in seq_len(nrow)) {
        for (n in seq_len(ncol)) {
          
          # Map physical plate coordinate to the parsed data index
          r_idx <- which(trt_info$row_idx == m)
          c_idx <- which(trt_info$col_idx == n)
          
          if (length(r_idx) > 0 && length(c_idx) > 0) {
            drug_entry <- trt_gnumber_conc[[r_idx, c_idx]]
          } else {
            drug_entry <- list() # Well was completely empty in D300 XML
          }
          
          if (length(drug_entry) >= j) {
            drug <- drug_entry[[j]][[1]]
            conc <- drug_entry[[j]][[2]]
            
            # Explicit vehicle fluids assigned zero
            if (drug %in% idfs$untreated_tags) {
              conc <- 0.0
            }
            
          } else {
            # If no drug is specified, assign untreated
            drug <- idfs$untreated_tags[[1]]
            conc <- 0.0
          }
          
          conc_mat[m, n] <- conc
          drug_mat[m, n] <- drug
        }
      }
      
      drug_data <- data.table::data.table(drug_mat)
      conc_data <- data.table::data.table(conc_mat)
      
      openxlsx::addWorksheet(wb, drug_sname)
      openxlsx::writeData(wb, sheet = (j * 2) - 1, drug_data, colNames = FALSE)
      openxlsx::addWorksheet(wb, conc_sname)
      openxlsx::writeData(wb, sheet = (j * 2), conc_data, colNames = FALSE)
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
#' This function parses a D300 *.tdd file (XML format) into a data.table
#'
#' @param D300_file string, file path to D300 .tdd file
#' @keywords D300
#'
#' @return data.table representing input \code{D300_file}.
#' 
#' @examples
#' td3 <- get_test_D300_data()
#' fs <- td3[["f_96w"]]
#' dose_df <- parse_D300_xml(fs[["d300"]])
#
#' @export
#'
parse_D300_xml <- function(D300_file) {
  assertthat::assert_that(is.character(D300_file), msg = "'D300_file' must be a character vector")
  assertthat::assert_that(assertthat::is.readable(D300_file), msg = "'D300_file' must be a readable path")
  
  # Open D300 XML format.
  D300_xml.tree <- XML::xmlTreeParse(D300_file, useInternal = TRUE) 
  top <- XML::xmlRoot(D300_xml.tree)
  
  # Safely retrieve units (prevents UseMethod error if node is missing).
  node_vol <- top[["VolumeUnit"]]
  vol_unit  <- if (!is.null(node_vol)) XML::xmlValue(node_vol) else NA
  
  node_conc <- top[["ConcentrationUnit"]]
  conc_unit <- if (!is.null(node_conc)) XML::xmlValue(node_conc) else NA
  
  node_mol <- top[["MolarityConcentrationUnit"]]
  mol_conc_unit <- if (!is.null(node_mol)) XML::xmlValue(node_mol) else NA
  
  # Handle missing ConcentrationUnit in newer D300 software versions
  if (is.na(conc_unit)) {
    conc_unit <- mol_conc_unit
  }
  
  # Assertions.
  if (!is.na(conc_unit) && !is.na(mol_conc_unit)) {
    assertthat::assert_that(conc_unit == mol_conc_unit, 
                            msg = "Mismatch between the units for ConcentrationUnit and MolarityConcentrationUnit")
  }
  
  # if there is DMSO backfill defined throw a warning, support not yet implemented
  backfills <- XML::xpathSApply(top, ".//Backfills/Backfill")
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
    
    # Safely extract information for every fluid (i.e. drugs) using XPath
    fluids <- XML::xpathSApply(xml_tree_root, ".//Fluids/Fluid")
    nfluids <- length(fluids)
    df_drug <- vector("list", nfluids)
    
    for (fi in seq_len(nfluids)) {
      fluid <- fluids[[fi]]
      id <- XML::xmlAttrs(fluid)[[id_col]]
      
      node_name <- fluid[["Name"]]
      name <- if (!is.null(node_name)) XML::xmlValue(node_name) else ""
      
      node_stock <- fluid[["Concentration"]]
      stock_conc <- if (!is.null(node_stock)) XML::xmlValue(node_stock) else NA
      
      node_conc_unit <- fluid[["ConcentrationUnit"]]
      fluid_conc_unit <- if (!is.null(node_conc_unit)) XML::xmlValue(node_conc_unit) else NA
      
      df_drug[[fi]] <- data.table::data.table(t(c(id, name, stock_conc, fluid_conc_unit)))
      colnames(df_drug[[fi]]) <- drug_cols
    }
    data.table::rbindlist(df_drug)
  }  


get_plate_info <- function(plate, vol_unit) {
  
  rows_plate <- XML::xmlValue(plate[["Rows"]])
  cols_plate <- XML::xmlValue(plate[["Cols"]])
  plate_dim <- sprintf("(%s,%s)", rows_plate, cols_plate)
  assay_vol <- XML::xmlValue(plate[["AssayVolume"]])
  desired_unit <- get_muL()
  assay_vol_conv <- convert_units(assay_vol, from = vol_unit, to = desired_unit)
  
  node_name <- plate[["Name"]]
  barcode_plate <- if (!is.null(node_name)) XML::xmlValue(node_name) else ""
  if (is.na(barcode_plate)) barcode_plate <- ""
  
  # check if the plate is randomized; should probably be changed
  node_rand <- plate[["Randomize"]]
  randomize <- if (!is.null(node_rand)) XML::xmlValue(node_rand) else ""
  if (!is.na(randomize) && randomize != "") {
    warning("Randomization of D300 plate possibly detected, but not supported yet.")
  }
  
  list(
    plate_dim = plate_dim,
    desired_unit = desired_unit,
    assay_vol_conv = assay_vol_conv,
    barcode_plate = barcode_plate
  )
}


get_D300_xml_treatments <-
  function(xml_tree_root, id_col = "ID", vol_unit, conc_unit) {
    
    # define treatment columns
    trt_cols <- c("D300_Plate_N", "D300_Barcode", "Dimension", "Row", "Col", 
                  "Volume", "Volume_Unit", id_col, "Concentration", "Unit")
    
    # extract drug dispensing information for each plate 
    plates <- XML::xpathSApply(xml_tree_root, ".//Plates/Plate")
    
    pl <- lapply(seq_along(plates), function(pli) {
      plate <- plates[[pli]]
      pl_info <- get_plate_info(plate, vol_unit) # plate info
      
      # extract drug dispensing information for each well using XPath
      wells <- XML::xpathSApply(plate, ".//Wells/Well")
      
      # Determine if indexing is 0-based or 1-based by checking the first row
      row_indices <- vapply(wells, function(w) {
        as.numeric(XML::xmlAttrs(w)[["Row"]])
      }, FUN.VALUE = numeric(1))
      offset <- if (length(row_indices) > 0 && min(row_indices) == 0) 1 else 0
      
      wl <- lapply(wells, function(well) {
        
        well_attr <- XML::xmlAttrs(well)
        row_well <- strtoi(well_attr[["Row"]]) + offset
        col_well <- strtoi(well_attr[["Col"]]) + offset
        
        # extract information each fluid delivered in well 
        fluids <- XML::xpathSApply(well, ".//Fluid")
        if (length(fluids) == 0) return(NULL)
        
        res <- vapply(fluids, function(fluid) {
          id_fluid <- XML::xmlAttrs(fluid)[[id_col]]
          conc_fluid <- XML::xmlValue(fluid)
          
          # define single entry
          c(
            pli,
            pl_info$barcode_plate,
            pl_info$plate_dim,
            row_well,
            col_well,
            pl_info$assay_vol_conv,
            pl_info$desired_unit,
            id_fluid,
            conc_fluid,
            conc_unit
          )
        }, character(length(trt_cols)))
        
        t(res) # transpose to correctly match columns
      })
      
      do.call(rbind, wl)
    })
    
    df_trt <- data.table::data.table(do.call(rbind, pl))
    colnames(df_trt) <- trt_cols
    df_trt
  }


get_conversion_factor <- function(from, to = get_muL()) {
  if (to != get_muL()) {
    stop(sprintf("conversion to unit '%s' not supported", to))
  }
  
  muL <- get_muL()
  switch(from,
         "nL" = 1e-3,
         muL = 1,
         "mL" = 1e3,
         stop(sprintf("unsupported conversion factor: '%s'", from))
  )
}


convert_units <- function(x, from, to) {
  conversion_factor <- get_conversion_factor(from, to) 
  as.double(x) * conversion_factor
}


#########
# Gnum
#########

parse_D300_metadata_file <- function(metadata_file) {
  if (tools::file_ext(metadata_file) %in% c("xls", "xlsx")) {
    D300_Gnum_sheets <- readxl::excel_sheets(metadata_file)
    nsheets <- length(D300_Gnum_sheets)
    
    # Assertions.
    assertthat::assert_that(is.character(metadata_file), msg = "'metadata_file' must be a character vector")
    assertthat::assert_that(assertthat::is.readable(metadata_file), msg = "'metadata_file' must be a readable path")
    
    if (nsheets != 1L) {
      futile.logger::flog.error("only one data sheet is supported, found '%s' sheets in '%s'",
                                nsheets, metadata_file)
    }
    
    metadata <- read_excel_to_dt(metadata_file,
                                 sheet = D300_Gnum_sheets[[1]],
                                 col_names = TRUE)
  } else {
    metadata <- data.table::fread(metadata_file, header = FALSE)
  }
  metadata
}


#########
# Utils
#########

fill_NA <- function(x, from, with) {
  idx <- is.na(x[[from]])
  if (any(idx)) {
    data.table::set(x, j = from, value = x[which(idx), ..with])
  }
  x
}

get_muL <- function() {
  # microLiter avoiding the use of non-ASCII characters for R CMD check
  paste0(rawToChar(as.raw(c(194, 181))), "L")
}
