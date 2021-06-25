#' standardize_record_values
#'
#' map values to a dictionary
#'
#' @param x a named array
#' @param dictionary a named array
#'
#' @return a named array with updated names
#' @export
#'
standardize_record_values <- function(x, dictionary = DICTIONARY) {
  for (i in seq_along(dictionary)) {
    x[x == names(dictionary[i])] <- dictionary[[i]]
  }
  x
}