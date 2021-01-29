#' Define Chi squared bins of variable width at each occasion
#'
#' @param eh encounter history object
#' @param n_bins integer, the number of bins to create
#' @param lambda estimate of density (lambda from poisson and exponential distributions)
#'
#' @return Data frame defining the bin intervals for each occasion
#' @export
#'
#' @examples build_gof_bins_var(eh, n_bins, lambda)
build_gof_bins_var <- function(eh, n_bins, lambda){
  # function to build bins for goodness of fit test with variable bin cutoffs
  # for every occasion
  
  # okay, so maybe it doesn't need to be it's own function.
  # but it works like this, it matches the other function, and I want to go home
  bin_data_raw <- purrr::map(eh$censor, gof_bins_var_int, lambda, n_bins)
    
  return(bin_data_raw)
}
