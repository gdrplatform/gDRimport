### helper functions ###
get_test_data1 <-
  function(data_dir = system.file(package = "gDRimport", "extdata", "data1")) {
    
    ddir <- system.file(package = "gDRimport", "extdata", "data1")
    
    ## define manifest ref data
    bcode_tbl <- expand.grid(c("201904190", "201904197"), letters[seq_len(6)])
    bcode_v <- paste0(bcode_tbl$Var1, bcode_tbl$Var2)
    templates_v <- c("Template_Untreated.xlsx", "Template_7daytreated.xlsx")
    clids_n <- c(rep(11, 2), rep(12, 2), rep(13, 2), rep(14, 2), rep(15, 2), rep(18, 2))
    ref_m_df <-
      data.frame(
        Barcode = bcode_v,
        Duration = rep(c(0, 168), 6),
        Template = rep(templates_v, 6),
        clid = paste0("CL000", clids_n)
      )
    
    list(
      m_file = file.path(ddir, "manifest.xlsx"),
      r_files =
        c(
          file.path(ddir, "RawData_day0.xlsx"),
          file.path(ddir, "RawData_day7.xlsx")
        ),
      t_files =
        c(
          file.path(ddir, "Template_7daytreated.xlsx"),
          file.path(ddir, "Template_Untreated.xlsx")
        ),
      ref_m_df = ref_m_df,
      ref_r1_r2 = file.path(ddir, "ref_RawData_day0_day7_xlsx.csv"),
      ref_r1 = file.path(ddir, "ref_RawData_day0_xlsx.csv"),
      ref_t1_t2 =  file.path(ddir, "ref_template_treated_untreated_xlsx.csv"),
      ref_t1 = file.path(ddir, "ref_template_treated_xlsx.csv")
    )
  }

get_test_data2 <-
  function(data_dir = system.file(package = "gDRimport", "extdata", "data2")) {
    ddir <- system.file(package = "gDRimport", "extdata", "data2")
    list(
      m_file = file.path(ddir, "manifest_Tecan_96_well_plates.xlsx"),
      r_files = file.path(ddir, "RawData_Tecan_96_well_plates.xlsx"),
      t_files =
        c(
          file.path(ddir, "D300_trt_Tecan_96_well_plates.xlsx")
        ),
      ref_m_df = file.path(ddir, "ref_manifest_Tecan_96_well_plates.RDS"),
      ref_r_df = file.path(ddir, "ref_RawData_Tecan_96_well_plates.RDS"),
      ref_t_df = file.path(ddir, "ref_D300_trt_Tecan_96_well_plates.RDS")
    )
  }

get_test_data3 <-
  function(data_dir = system.file(package = "gDRimport", "extdata", "data3")) {
    ddir <- system.file(package = "gDRimport", "extdata", "data3")
    list(
      d300_96w_file = file.path(ddir, "D300_96_well_plate_example.tdd"),
      d300_384w_file = file.path(ddir, "D300_384_well_plate_example.tdd"),
      Gnum_96w_file = file.path(ddir, "Gnumber_D300_96_well_plate.xlsx"),
      Gnum_384w_file = file.path(ddir, "Gnumber_D300_384_well_plate.xlsx"),
      ref_Gnum_96w_file = file.path(ddir, "ref_Gnumber_D300_96_well_plate.RDS"),
      ref_Gnum_384w_file = file.path(ddir, "ref_Gnumber_D300_384_well_plate.RDS"),
      ref_d300_96w_file = file.path(ddir, "ref_D300_96_well_plate_example.RDS"),
      ref_d300_384w_file = file.path(ddir, "ref_D300_384_well_plate_example.RDS"),
      dest_path_d300_96w = file.path(ddir, "output_files_96w", "output"),
      dest_path_d300_384w = file.path(ddir, "output_files_384w", "output"),
      ref_output_path_d300_96w = file.path(ddir, "output_files_96w", "reference"),
      ref_output_path_d300_384w = file.path(ddir, "output_files_384w", "reference")
    )
  }
