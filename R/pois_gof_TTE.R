#' Poisson goodness-of-fit tests for TTE
#'
#' @param n_bins a vector of the number of bins to use in each chi squared test
#' @param lambda estimate of density calculated from the tte_estN_fn output
#' @param eh encounter history from tte_build_eh
#'
#' @return Returns a summary table for the tests and the chi squared table from each test
#' @export
#'
#' @examples pois_gof_TTE(n_bins, lambda, eh)
pois_gof_tte <- function(n_bins, lambda, eh){
  
  tmp <- purrr::map(n_bins, pois_gof_test, count = eh$count, lambda = lambda)
  
  out <- list(
    summary = bind_rows(purrr::map(
      .x = tmp, 
      .f = function(x) x$summary
    )),
    chi_sq_tables = purrr::map(
      .x = tmp,
      .f = function(x) x$exp_obs
    )
  )
  
  return(out)
  
}
