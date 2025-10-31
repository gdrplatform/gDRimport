#' @importFrom utils adist
#' @importFrom magrittr %>%
#' @importFrom tibble tribble
#' @importFrom methods new slotNames
#' @importFrom data.table := setDF
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
      "..with",
      ".N",
      ".SD",
      "Dose",
      "column_name",
      "name",
      "new_name",
      "value",
      "ccle_name",
      ".",
      "CCLEName",
      "OncotreeLineage",
      "row_name",
      "ReadoutValue",
      "Elapsed"
    ),
    utils::packageName())
}
