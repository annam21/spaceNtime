exp_gof_test <- function(eh, n_bins, lambda, bin_cuts = "equal"){
  
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
 
  assertthat::assert_that(
    n_bins%%1 == 0,
    n_bins >= 2,
    msg = "n_bins must be an integer greater than or equal to 2"
  )
  
  # Build bin data, throw error if bin_cuts are not correctly defined
  if(bin_cuts == "equal"){
    bin_data <- build_gof_bins_eq(eh, n_bins, lambda)
  }else{
    if(bin_cuts == "variable"){
      bin_data <- build_gof_bins_var(eh, n_bins, lambda)
  }else{
    stop("bin_cuts must be the words `equal` or `positive` as a character")
  }
  }
  
  tmp1 <- tibble(bin = 1:n_bins, n_0 = 0)
  
  tmp2 <- eh %>%
    mutate(bin = unlist(map2(bin_data, eh$STE, assign_bins))) %>%
    group_by(bin) %>%
    tally() %>%
    right_join(tmp1) %>%
    mutate(n = n + n_0) %>%
    select(bin, n) %>%
    mutate(n = tidyr::replace_na(n, 0)) %>%
    arrange(bin)

  
  
 
  
  
}
