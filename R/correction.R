#' Get Excel sheets
#'
#' get sheets for given set of XLS files
#' 
#' @param files charvec with file paths
#' @keywords correction_exception
#'
#' @return named list where names are the excel filenames
#' and the values are the sheets within each file
#' 
get_xl_sheets <- function(files) {
  checkmate::assert_character(files)
  template_sheets <- lapply(files, readxl::excel_sheets)
  names(template_sheets) <- files
  template_sheets
}


#' Evaluate if template file with single sheet is present,
#' if the name of the sheet is correct and if it can be fixed
#' 
#' get sheets for given set of XLS files
#' @param ts list with template sheets info
#' @keywords correction_exception
#' 
#' @return logical flag
#' 
.check_against_single_template_sheet <- function(ts) {

  checkmate::assert_list(ts)
  # edge case: 'untreated' template with (1) single sheet
  # and (2) improperly named (not 'idfs[['drug']]) 
  myv <- vapply(ts, function(x) {
    length(x) == 1 && x != gDRutils::get_env_identifiers("drug")
  }, logical(1))
 
  # one template file with 
  status <- any(myv) && !all(myv)
  if (isTRUE(status)) {
    attributes(status) <- list(file = names(myv[myv]))
  }
  status
}


### XLS templates validation

#' Correct names of the template sheets (if required)
#' 
#' Correct names of the template sheets (if required)
#' 
#' @param tfiles charvec with paths to template files
#' @keywords correction_exception
#' 
#' @return charvec with paths to corrected sheet names
correct_template_sheets <- function(tfiles) {

  checkmate::assert_character(tfiles)

  ts <- get_xl_sheets(tfiles)
  # no issues with templates sheets, return input data
  if (!are_template_sheets_valid(ts)) {
    # iterate through available fixes to try to fix data
    # spaces/bad capitalization 
    ts <- fix_typos_with_reference(ts, unlist(gDRutils::get_env_identifiers()))
    
    # additional prefixes/postfixes in the optional sheets (e.g. "Concentration_2_3", "my_gnumber_2)"
    ts <- fix_typos_with_reference(ts, get_expected_template_sheets("optional"), method = "grepl")
   
    # additional prefixes/postfixes in the optional sheets and/or missing underscores
    ts <-
      fix_typos_with_reference(
        ts,
        get_expected_template_sheets("optional"),
        method = "grepl",
        fix_underscores = TRUE
      )
    # check one letter typos
    ts <- fix_typos_with_reference(
      ts,
      unlist(gDRutils::get_env_identifiers(gDRutils::get_required_identifiers(), simplify = FALSE)),
      method = "adist")
    
    # check if there is a template file with single, improperly named sheet 
    # fix if possible
    st1 <- .check_against_single_template_sheet(ts)
    if (st1) {
      inv_file <- attributes(st1)[["file"]]
      ts[[inv_file]] <- gDRutils::get_env_identifiers("drug")
    }
  }
  stopifnot(are_template_sheets_valid(ts))
  ts
}

#' Get names of the sheets expected in templates xlsx
#'
#' Get names of the sheets expected in templates xlsx
#'
#' @param type charvec type of the sheets
#' @keywords correction_exception
#' 
#' @return string with type of the sheets
#' 
get_expected_template_sheets <-
  function(type = c("all", "core", "optional")) {

    type <- match.arg(type)
    
    ctype <- as.character(gDRutils::get_env_identifiers(c("drug",
                                                          "concentration"),
                                                        simplify = FALSE))
    otype <- as.character(gDRutils::get_env_identifiers(
      c("drug2",
        "drug3",
        "concentration2",
        "concentration3"),
      simplify = FALSE
    ))
    
    if (type == "all") {
      c(ctype, otype)
    } else if (type == "core") {
      ctype
    } else if (type == "optional") {
      otype
    } else {
      stop(sprintf("bad type: '%s", type))
    }
  }

#' are template sheet valid?
#' 
#' are template sheet valid?
#' 
#' @param ts list with (per file) template sheets
#' @keywords correction_exception
#' 
#' @seealso get_xl_sheets
#' 
#' @return logical flag
are_template_sheets_valid <- function(ts) {

  checkmate::assert_list(ts)

  cl <- vector("list", 10)
  drug <- gDRutils::get_env_identifiers("drug")

  # only allowed sheet names are present
  myv <- vapply(ts, function(x) {
    all(drug %in% x)
  }, logical(1))
  if (sum(!myv) == 1 && length(myv) > 1) { # allow for having only `Drug` sheet in case of untreated template
    myv[!myv] <- drug %in% ts[!myv][[1]]
  }
  cl[[length(cl) + 1]] <- all(myv)
  
 # at least idfs[['drug']] sheet is present in all files
  myv2 <- vapply(ts, function(x) {
    any(gDRutils::get_env_identifiers("drug") %in% x)
  }, logical(1))
  cl[[length(cl) + 1]] <- all(myv2)
  
 all(unlist(cl))
}


### TYPOS ###

#' Fix typos using reference data
#' 
#' Fix typos using reference data
#' Evaluate given list of ids and try to update them 
#  if they are similar to any id from 'ref' data
#' 
#' @param data list of charvec(s) or charvec with data
#' @param ref charvec with reference data
#' @param method charvec type of the method to be used
#' 'exact' is used to find  identical entries from 'ref' in the 
#' data (after corrections and uppercase'ing)
#' 'grepl' is used to find entries from 'ref' that might be 
#' somehow pre- or post- fixed
#' @param fix_underscores logical flag fix the issues with underscores in data identifiers? 
#' @keywords correction_exception
#' 
#' @return list or charvec with corrected data
#' 
fix_typos_with_reference <-
  function(data,
           ref,
           method = c("exact", "grepl", "adist"),
           fix_underscores = FALSE) {
    
    stopifnot(is.list(data) || is.character(data))
    checkmate::assert_character(ref)
    method <- match.arg(method)
    checkmate::assert_flag(fix_underscores)
    
    out <- if (is.list(data)) {
      lapply(data, function(x) {
        fix_typos_with_reference(x, ref, method, fix_underscores)
      })
    } else {
      # remove spaces at the beginning/end
      cdata <- vapply(data, function(x) gsub("^ +| +$", "", x), character(1))
      
      # replace spaces with "_"
      cdata <- vapply(cdata, function(x) gsub(" +", "_", x), character(1))
      
      # update to valid identifier (if found)
      # convert both v and valid identifiers to upper case before comparison
      # remove "_" from references if 'fix_underscores' enabled
      ref_uc <- if (fix_underscores) {
        gsub("_", "", toupper(ref))
      } else {
        toupper(ref)
      }
      
      cdata <- vapply(cdata, function(x) {
        idx <- if (method == "exact") {
          which(ref_uc %in% toupper(x))
        } else if (method == "adist") {
          which(adist(toupper(x), ref_uc) == 1)
        } else {
          which(mgrepl(ref_uc, toupper(x)))
        }
        if (length(idx) == 1) {
          ref[idx]
        } else {
          x
        }
      }, character(1))
      cdata
    }
    out
  }

#' grep wrapper to support multiple patterns
#' 
#' @param patterns charvec with patterns to be checked
#' @param x charvec with data
#' @param do_unlist logical_flag unlist the final results?
#' @param ... additional argument
#' @keywords correction_exception
#' 
#' @return list of charvec with grep output
#' 
mgrepl <- function(patterns, x, do_unlist = TRUE,  ...) {
  checkmate::assert_character(patterns)
  checkmate::assert_character(x)
  checkmate::assert_flag(do_unlist)
  
  out <- lapply(patterns, function(p) {
    grepl(p, x, ...)
  })
  if (do_unlist) {
    unlist(out)
  }
}

