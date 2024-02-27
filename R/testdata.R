#' get primary test data
#'
#' @examples 
#' get_test_data()
#' 
#' @keywords test_data_class
#' @export
#'
#' @return object class "gdr_test_data" with with input data (manifest/template/result paths)
#' and related reference data (qs file paths)
get_test_data <- function() {
    
    ddir <- system.file(package = "gDRimport", "extdata", "data1")
    
    ## define manifest ref data
    bcode_tbl <- expand.grid(c("201904190", "201904197"), letters[seq_len(6)])
    bcode_v <- paste0(bcode_tbl$Var1, bcode_tbl$Var2)
    templates_v <- c("Template_Untreated.xlsx", "Template_7daytreated.xlsx")
    clids_n <- c(rep(11, 2), rep(12, 2), rep(13, 2), rep(14, 2), rep(15, 2), rep(18, 2))
    ref_m_df <-
      data.table::data.table(
        Barcode = bcode_v,
        Duration = rep(c(0, 168), 6),
        Template = rep(templates_v, 6),
        clid = paste0("CL000", clids_n)
      )
    
    new("gdr_test_data",
        manifest_path = file.path(ddir, "manifest.xlsx"),
        result_path =
          c(
            file.path(ddir, "RawData_day0.xlsx"),
            file.path(ddir, "RawData_day7.xlsx")
          ),
        template_path =
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

#' get test Tecan data
#'
#' @examples 
#' get_test_Tecan_data()
#
#' @keywords test_data
#' @export
#'
#' @return list with with input data (manifest/template/result paths)
#' and related reference data (qs file paths)
get_test_Tecan_data <- function() {
    ddir <- system.file(package = "gDRimport", "extdata", "data2")
    list(
      m_file = file.path(ddir, "manifest_Tecan_96_well_plates.xlsx"),
      r_files = file.path(ddir, "RawData_Tecan_96_well_plates.xlsx"),
      t_files =
        c(
          file.path(ddir, "D300_trt_Tecan_96_well_plates.xlsx")
        ),
      ref_m_df = file.path(ddir, "ref_manifest_Tecan_96_well_plates.qs"),
      ref_r_df = file.path(ddir, "ref_RawData_Tecan_96_well_plates.qs"),
      ref_t_df = file.path(ddir, "ref_D300_trt_Tecan_96_well_plates.qs")
    )
  }

#' get test D300 data
#'
#' @examples 
#' get_test_D300_data()
#
#' @keywords test_data
#' @export
#'
#' @return list with with input data (manifest/template/result paths)
#' and related reference data (qs file paths)
get_test_D300_data <- function() {
    ddir <- system.file(package = "gDRimport", "extdata", "data3")
    list(
      f_96w = list(
        d300 = file.path(ddir, "D300_96_well_plate_example.tdd"),
        Gnum = file.path(ddir, "Gnumber_D300_96_well_plate.xlsx"),
        dest_path = file.path(ddir, "output_files_96w", "output"),
        ref_d300 = file.path(ddir, "ref_D300_96_well_plate_example.qs"),
        ref_Gnum = file.path(ddir, "ref_Gnumber_D300_96_well_plate.qs"),
        ref_output_path = file.path(ddir, "output_files_96w", "reference")
      ),
      f_384w = list(
        d300 = file.path(ddir, "D300_384_well_plate_example.tdd"),
        Gnum = file.path(ddir, "Gnumber_D300_384_well_plate.xlsx"),
        ref_Gnum = file.path(ddir, "ref_Gnumber_D300_384_well_plate.qs"),
        ref_d300 = file.path(ddir, "ref_D300_384_well_plate_example.qs"),
        dest_path = file.path(ddir, "output_files_384w", "output"),
        ref_output_path = file.path(ddir, "output_files_384w", "reference")
      )
    )
  }

#' get test EnVision data
#'
#' @examples 
#' get_test_EnVision_data()
#
#' @keywords test_data
#' @export
#'
#' @return list with with input data (manifest/template/result paths)
#' and related reference data (.qs file paths)
get_test_EnVision_data <- function() {
    ddir <- system.file(package = "gDRimport", "extdata", "data4")
    fls <- list.files(ddir, full.names = TRUE)
    list(
      m_file = grep("\\/Manifest.xlsx$", fls, value = TRUE),
      r_files = grep("\\/[H|K].+.csv$", fls, value = TRUE),
      t_files = grep("\\/Project40.+.xlsx$", fls, value = TRUE),
      ref_l_path = file.path(ddir, "ref_l.qs")
    )
  }
