exp_gof_test <- function(eh, n_bins, lambda, bin_cuts = "fixed"){
  
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
  if(bin_cuts == "fixed"){
    bin_data <- build_gof_bins_eq(eh, n_bins, lambda)
  }else{
    if(bin_cuts == "variable"){
      bin_data <- build_gof_bins_var(eh, n_bins, lambda)
  }else{
    stop("bin_cuts must be the word `fixed` or the word `variable` as a character")
  }
  }
  
  tmp1 <- tibble(bin = 1:n_bins, n_0 = 0)
  
  probs <- bind_rows(bin_data) %>%
    group_by(bin) %>%
    summarise(prob_mean = mean(prob), .groups = "drop")
  cuts <- bind_rows(bin_data) %>%
    group_by(bin) %>%
    summarise(cut_mean = mean(left), .groups = "drop")
  
  chi_sq_data <- eh %>%
    mutate(bin = unlist(map2(bin_data, eh$STE, assign_bins))) %>%
    group_by(bin) %>%
    tally() %>%
    right_join(tmp1, by = "bin") %>%
    mutate(n = n + n_0) %>%
    select(bin, n) %>%
    mutate(n = tidyr::replace_na(n, 0)) %>%
    arrange(bin) %>%
    full_join(probs, by = "bin")

  tmp3 <- chisq.test(x = chi_sq_data$n, p = chi_sq_data$prob_mean, simulate.p.value = T)
  
  out <- list(
    summary = tibble(
      n_bins = n_bins, 
      X_squared = tmp3$statistic, 
      p = tmp3$p.value, 
      lambda = lambda),
    exp_obs = tibble(
      bin = chi_sq_data$bin,
      bin_cut = cuts$cut_mean,
      observed = tmp3$observed,
      expected = tmp3$expected,
      residuals = tmp3$residuals
    )
    )
  
  return(out)
}




