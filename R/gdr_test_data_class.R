#' gDR Test Data object 
#' 
#' Object class `gdr_test_data` is build by function [gDRimport::get_test_data()]
#' 
#' @slot manifest_path character, path to manifest file
#' @slot result_path character, path(s) to results file
#' @slot template_path character, path(s) to data.table with template data
#' @slot ref_m_df character, data.table with manifest data
#' @slot ref_r1_r2 character, path to reference file with raw data for treated & untreated 
#' @slot ref_r1 character, path to reference file with raw data for treated
#' @slot ref_t1_t2 character, path to reference template file with treated & untreated data
#' @slot ref_t1 character, path to reference template file with treated data
#' 
#' @return object class `gdr_test_data` with primary test data
#' 
#' @docType class
#' @name gdr_test_data-class
#' @keywords classes
#' 
#' @export
setClass(
  Class = "gdr_test_data",
  package = "gDRimport",
  slots = list(
    manifest_path = "character",
    result_path = "character",
    template_path = "character",
    ref_m_df = "data.table",
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
#' @return value of slot `manifest_path`
#' 
#' @examples
#'  td <- get_test_data()
#'  manifest_file_path <- manifest_path(td)
#' 
#' @docType methods
#' @rdname manifest_path-method
#' @keywords methods
#' 
#' @export
setGeneric("manifest_path", function(x) standardGeneric("manifest_path"))

#' @aliases manifest_path,gdr_test_data-method
#' @rdname manifest_path-method
setMethod("manifest_path", "gdr_test_data", function(x) x@manifest_path)

#' Method result_path
#'
#' Method for object gdr_test_data - access to slot `result_path`
#' @param x object class gdr_test_data
#' 
#' @return value of slot `result_path`
#' 
#' @examples
#'  td <- get_test_data()
#'  result_file_path <- result_path(td)
#'   
#' @docType methods
#' @rdname result_path-method
#' @keywords methods
#' 
#' @export
setGeneric("result_path", function(x) standardGeneric("result_path"))

#' @aliases result_path,gdr_test_data-method
#' @rdname result_path-method
setMethod("result_path", "gdr_test_data", function(x) x@result_path)

#' Method template_path
#'
#' Method for object gdr_test_data - access to slot `template_path`
#' @param x object class gdr_test_data
#'
#' @return value of slot `template_path`
#' 
#' @examples
#'  td <- get_test_data()
#'  template_file_path <- template_path(td)
#'  
#' @docType methods
#' @rdname template_path-method
#' @keywords methods
#' 
#' @export
setGeneric("template_path", function(x) standardGeneric("template_path"))

#' @aliases template_path,gdr_test_data-method
#' @rdname template_path-method
setMethod("template_path", "gdr_test_data", function(x) x@template_path)
