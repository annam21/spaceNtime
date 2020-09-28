#' Exponential goodness-of-fit tests for TTE
#'
#' @param eh encounter history from tte_build_eh
#' @param n_bins a vector of the number of bins to use in the chi squared tests
#' @param lambda Estimate of density calculated from the output of tte_estn_fn
#' @param bin_cuts "fixed" or "variable". Fixed bins have the same cutoff values for every occasion. Variable bins allow the cutoff values to change to keep the bin widths equal.
#'
#' @return Returns a summary table showing the results of all of the chi squared tests and the chi squared table for each test.
#' @export
#'
#' @examples exp_gof_TTE(eh, n_bins, lambda, bin_cuts = "fixed")
exp_gof_TTE <- function(eh, n_bins, lambda, bin_cuts = "fixed"){
  
  
  eh <- eh %>%
    mutate(to_event = TTE) %>%
    select(occ, cam, start, end, to_event, censor)
  
  tmp <- purrr::map(n_bins, exp_gof_test, eh = eh, lambda = lambda, bin_cuts = bin_cuts)
  
  out<- list(
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
