# Helper functions
#' Replace negative values with NA
#'
#' Retains 0 and positive values, and replaces negative values with NA
#'
#' @param x A numeric vector
#'
#' @return A numeric vector of the same length as x
#' @export
#'
#' @examples
#' x <- seq(-2, 2, by = 1)
#' only_pos(x)
only_pos <- function(x) {
  out <- replace(x, x < 0, NA)
  return(out)
}
