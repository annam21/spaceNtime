#' Exponential goodness-of-fit tests
#'
#' @param eh encounter history df with the columns required by spaceNtime
#' @param n_bins the number of bins to use in the chi squared test
#' @param lambda estimate of lambda calculated from the estN function
#' @param bin_cuts "fixed" or "variable" Fixed bins use the same cutoff for each occasion. Variable bins use different cutoffs but keep the width of bins equal within occasions.
#'
#' @return Returns a summary table of all goodness of fit tests and the chi squared results for each test
#' @export
#'
#' @examples exp_gof_fn(eh, n_bins, lambda = NULL, bin_cuts = "fixed")
exp_gof_fn <- function(eh, n_bins, lambda = NULL, bin_cuts = "fixed"){
  
  tmp <- map(n_bins, exp_gof_test, eh = eh, lambda = lambda, bin_cuts = bin_cuts)
  
  out<- list(
    summary = bind_rows(map(
      .x = tmp, 
      .f = function(x) x$summary
      )),
    tables = map(
      .x = tmp,
      .f = function(x) x$exp_obs
    )
  )

  return(out)
}

# x <- exp_gof_fn(eh, n_bins, lambda, area = NULL, bin_cuts = "fixed")














