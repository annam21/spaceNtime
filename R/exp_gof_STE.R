
#' Exponential goodness-of-fit tests for STE
#'
#' @param eh encounter history df with the columns required by spaceNtime
#' @param n_bins the number of bins to use in the chi squared test
#' @param lambda estimate of lambda calculated from the ste_estn_fn output
#' @param bin_cuts "fixed" or "variable". Fixed bins use the same cutoff for each occasion. Variable bins use different cutoffs but keep the width of bins equal within occasions.
#'
#' @return Returns a summary table of all goodness of fit tests and the chi squared results for each test
#' @export
#'
#' @examples exp_gof_STE(eh, n_bins, lambda = NULL, bin_cuts = "fixed")
exp_gof_ste <- function(n_bins, lambda = NULL, eh, bin_cuts = "fixed"){
  
  # Don't think I'm going to use this here. It was originally going to be an option
  # to allow the user to specify either lambda or the data necessary to estimate 
  # abundance and calculate lambda
  # assertthat::assert_that(
  #   (is.null(lambda) + is.null(area)) == 1, 
  #   msg = "you must supply a value for either lambda or area, not both")
  # 
  # if(is.null(lambda)){
  #   
  #   lambda <- (stuff here)
  # }
  
  assertthat::assert_that(is.data.frame(eh))
  assertthat::assert_that(
    assertr::verify(eh,
                    assertr::has_all_names("occ", "start", "end", "STE", "censor"), 
                    success_fun = success_logical, 
                    error_fun = error_logical), 
    msg = "eh must be a dataframe with columns occ, start, end, STE and censor")
  
  assertthat::assert_that(
    is.numeric(lambda), 
    length(lambda) == 1, 
    lambda > 0,
    msg = "lambda must be a single numeric value greater than 0"
  )
  
  eh <- eh %>%
    mutate(to_event = STE) %>%
    select(occ, start, end, to_event, censor)
  
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

# x <- exp_gof_fn(eh, n_bins, lambda, area = NULL, bin_cuts = "fixed")














