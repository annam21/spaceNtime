#' Calculate exponential density between two values
#'
#' @param left leftmost values
#' @param right righmost values
#' @param lambda lambda, from the exponential distribution
#'
#' @return returns value between 0 and 1
#' @export
#'
#' @examples exp_dens(0.1, 0.2, 1)
exp_dens <- function(left, right, lambda){
  pleft <- pexp(left, lambda); pright <- pexp(right, lambda)
  out <- pright - pleft
  return(out)
}
