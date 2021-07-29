#' is_readable_v
#' Check if all paths in vector are readable
#'
#' @param paths a character with path(s)
#'
#' @return
#' @export
#'
is_readable_v <- function(paths) {
  checkmate::assert_character(paths)
  missing_path_string <- paste(paths[as.logical(-file.access(paths, 4))], collapse = ", ", sep = "   ")
  message <- paste0("Following path(s) with no read permission found: '", missing_path_string, "'")
  assertthat::assert_that(sum(file.access(paths, 4)) == 0, msg = message)
}

