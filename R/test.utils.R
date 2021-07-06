#' standardize_df
#'
#' Transform all the columns in a dataframe to character type
#'
#' @param df a dataframe for standardization
#'
#' @return a standardized dataframe
#' @export
#'
standardize_df <- function(df) {
  # Assertions:
  stopifnot(inherits(df, "data.frame"))
  data.frame(lapply(df, as.character))
}
