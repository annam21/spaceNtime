#' Poisson density function
#'
#' @param x count
#' @param lambda parameter of the poisson distribution
#'
#' @return returns the probability of count x
#' @export
#'
#' @examples pois_dens(3,1)
pois_dens <- function(x, lambda){
  out <- lambda^x * exp(-lambda) / factorial(x)
}