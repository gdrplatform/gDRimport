#' gDR Test Data object 
#' 
#' Object class `gdr_test_data` is build by function [gDRimport::get_test_data()]
#'
#' @name gdr_test_data
#' @slot manifest_path character, path to manifest file
#' @slot result_path character, path(s) to results file
#' @slot template_path character, path(s) to data.frame with template data
#' @slot ref_m_df character, data.frame with manifest data
#' @slot ref_r1_r2 character, path to reference file with raw data for treated & untreated 
#' @slot ref_r1 character, path to reference file with raw data for treated
#' @slot ref_t1_t2 character, path to reference template file with treated & untreated data
#' @slot ref_t1 character, path to reference template file with treated data
#' 
#' @return object class `gdr_test_data` with primary test data
#' @export
setClass(
  Class = "gdr_test_data",
  package = "gDRimport",
  slots = list(
    manifest_path = "character",
    result_path = "character",
    template_path = "character",
    ref_m_df = "data.frame",
    ref_r1_r2 = "character",
    ref_r1 = "character",
    ref_t1_t2 = "character",
    ref_t1 = "character"
  )
)

# Basic method show
setMethod("show", "gdr_test_data",
          function(object) {
            cat("class:", class(object)[1], "\nslots:", slotNames(object))
          }
)

#' Method manifest_path
#' 
#' Method for object gdr_test_data - access to slot `manifest_path`
#' @param x object class gdr_test_data
#'
#' @export
setGeneric("manifest_path", function(x) standardGeneric("manifest_path"))
setMethod("manifest_path", "gdr_test_data", function(x) x@manifest_path)

#' Method result_path
#'
#' Method for object gdr_test_data - access to slot `result_path`
#' @param x object class gdr_test_data
#' 
#' @export
setGeneric("result_path", function(x) standardGeneric("result_path"))
setMethod("result_path", "gdr_test_data", function(x) x@result_path)

#' Method template_path
#'
#' Method for object gdr_test_data - access to slot `template_path`
#' @param x object class gdr_test_data
#'
#' @export
setGeneric("template_path", function(x) standardGeneric("template_path"))
setMethod("template_path", "gdr_test_data", function(x) x@template_path)
