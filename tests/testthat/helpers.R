### helper fucnctions ###
get_test_data1 <-
  function(data_dir = system.file(package = "gDRimport", "extdata", "data1")) {
    
    ddir <- system.file(package = "gDRimport", "extdata", "data1")
    
    ## define manifest ref data
    bcode_tbl <- expand.grid(c("201904190","201904197"), letters[1:6])
    bcode_v <- paste0(bcode_tbl$Var1, bcode_tbl$Var2)
    templates_v <- c("Template_Untreated.xlsx", "Template_7daytreated.xlsx")
    clids_n <- c(rep(11, 2), rep(12, 2), rep(13, 2), rep(14, 2), rep(15, 2), rep(18, 2))
    ref_m_df <- data.frame(Barcode = bcode_v, Duration = rep(c(0, 168), 6), Template = rep(templates_v, 6), clid = paste0("CL000",clids_n))
    
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
