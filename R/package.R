#' @importFrom utils adist read.table write.csv write.table
#' @importFrom magrittr %>%
#' @importFrom tibble tribble
#' @importFrom methods new slotNames
NULL

DICTIONARY <- list(
  Untreated = "untreated",
  Vehicle = "vehicle",
  vehcle = "vehicle"
)

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "..barcode_col",
      "..selected_cols",
      "..with"
    ),
    utils::packageName())
}
