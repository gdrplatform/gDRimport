#' Parse_D300
#'
#' This function pares a D300 *.tdd file (XML format)
#'
#' @param D300_file string, file path to D300 .tdd file
#
#' @export
#'
#'#parse_D300 takes a D300 file in .tdd format (XML) and returns a data.frame
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
    fluids  <- XML::xpathSApply(top, ".//Fluids", XML::xmlChildren)
    nfluids <- length(fluids)
    drug_cols <- c('ID', 'Name', 'Stock_Conc', 'Stock_Unit')
    df_drug <- data.frame(matrix(ncol = length(drug_cols), nrow = 0))
    colnames(df_drug)<-drug_cols 
    for (fi in 1:nfluids) {
      id <- XML::xmlAttrs(top[['Fluids']][fi][['Fluid']])
      name <- XML::xmlValue(top[['Fluids']][fi][['Fluid']][['Name']])
      stock_conc <- XML::xmlValue(top[['Fluids']][fi][['Fluid']][['Concentration']])
      conc_unit <- XML::xmlValue(top[['Fluids']][fi][['Fluid']][['ConcentrationUnit']])
      df_drug_entry <- data.frame(t(c(id, name, stock_conc, conc_unit)))
      colnames(df_drug_entry) <- drug_cols
      df_drug <- rbind(df_drug, df_drug_entry)
    }
    #convert unit volumes to uL 
    vol_unit  <- XML::xmlValue(top[['VolumeUnit']])
    if (vol_unit == 'nL') {
      vol_conversion<- 1e-3;
      vol_unit_conv='µL'
    }else if(volumeUnit == 'µL') {
      vol_conversion<- 1;
      vol_unit_conv='µL'
    }else if (volumeUnit == 'mL') {
      vol_conversion<- 1e3;
      vol_unit_conv='µL'
    }else{
      stop(sprintf("Error with %s unit in VolumeUnit, not supported.",vol_unit))
    }
    # ff there is DMSO backfill defined throw a warning, support not yet implemented
    backfills <- XML::xpathSApply(top, ".//Backfills/Backfill", XML::xmlChildren)
    if (length(backfills)>0){
      warning('Backfill identified in D300 but not supported.')
    }
    #initialize data.frame for treatments
    trt_cols <- c('D300_Plate_N', 'D300_Barcode', 'Dimension', 'Row', 'Col', 'Volume','Volume_Unit', 'ID', 'Concentration', 'Unit')
    df_trt <- data.frame(matrix(ncol = length(trt_cols), nrow = 0))
    colnames(df_trt)<-trt_cols 
    #extract drug dispensing information for each plate 
    plates <- XML::xpathSApply(top, ".//Plates", XML::xmlChildren)
    nplates <- length(plates)
    for (pi in 1:nplates){
      #plate info
      rows_plate <- XML::xmlValue(top[['Plates']][pi][['Plate']][['Rows']])
      cols_plate <- XML::xmlValue(top[['Plates']][pi][['Plate']][['Cols']])
      plate_dim <- paste('(',rows_plate,',',cols_plate,')', sep='')
      assay_vol <- as.double(XML::xmlValue(top[['Plates']][pi][['Plate']][['AssayVolume']]))
      assay_vol_conv <- as.double(assay_vol) * vol_conversion
      barcode_plate <- XML::xmlValue(top[['Plates']][pi][['Plate']][['Name']])
      #this check if the plate is randomize should probably be changed 
      randomize <- XML::xmlValue(top[['Plates']][pi][['Plate']][['Randomize']])
      if (randomize!=''){
        warning('Randomization of D300 plate possibly detected, but not supported yet.')
      }
      
      #extract drug dispensing information for each well 
      wells<- XML::xpathSApply(top[['Plates']][pi][['Plate']], ".//Wells", XML::xmlChildren)
      nwells <- length(wells)
      for (wi in 1:nwells){
        #indexes of row and col start from zero, so add one
        well_attr <-XML::xmlAttrs(top[['Plates']][pi][['Plate']][['Wells']][wi][['Well']])
        row_well <- strtoi(well_attr[['Row']]) +1
        col_well = strtoi(well_attr[['Col']]) +1
        
        #extract information each fluid delivered in well 
        fluids <- XML::xpathSApply(top[['Plates']][pi][['Plate']][['Wells']][wi][['Well']], ".//Fluid", XML::xmlChildren)
        nfluids <- length(fluids)
        for (fi in 1:nfluids){
          id_fluid <- XML::xmlAttrs(top[['Plates']][pi][['Plate']][['Wells']][wi][['Well']][[fi]])
          conc_fluid <-  XML::xmlValue(top[['Plates']][pi][['Plate']][['Wells']][wi][['Well']][[fi]])
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
    df_D300<-merge(df_trt, df_drug, by.x='ID', by.y='ID', all.x=TRUE)
    return(df_D300)
  }




