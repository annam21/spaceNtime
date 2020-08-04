
exp_gof_fn <- function(eh, n_bins, lambda = NULL, area = NULL, bin_cuts = "fixed"){
  
  # assertthat::assert_that(
  #   (is.null(lambda) + is.null(area)) == 1, 
  #   msg = "you must supply a value for either lambda or area, not both")
  # 
  # if(is.null(lambda)){
  #   
  #   lambda <- (stuff here)
  # }
  
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

x <- exp_gof_fn(eh, n_bins, lambda, area = NULL, bin_cuts = "fixed")














