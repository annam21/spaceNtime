#' Single iteration of the poisson goodness-of-fit test
#'
#' @param n_bins number of bins to use in chi-squared test
#' @param count vector of observed counts
#' @param lambda estimate of lambda
#'
#' @return returns the result of the goodness-of-fit test
#' @export
#'
#' @examples pois_gof_test(n_bins, count, lambda)
pois_gof_test <- function(n_bins, count, lambda){
  
  data <- tibble(
    occ = 1:length(count),
    count = count
  )
  
  tmp1 <- tibble(bin = 1:n_bins, n_0 = 0)
  
  data_gof <- data %>%
    mutate(bin = count + 1) %>%
    mutate(bin = replace(bin, bin > n_bins, n_bins)) %>%
    group_by(bin) %>%
    tally()  %>%
    right_join(tmp1, by = "bin") %>%
    mutate(n = n + n_0) %>%
    select(bin, n) %>%
    mutate(n = tidyr::replace_na(n, 0)) %>%
    arrange(bin)
  
  chi_sq_data <- tibble(
    bin = 1:n_bins, 
    prob = pois_dens(bin - 1, lambda)
  ) %>%
    full_join(data_gof, by = "bin") %>%
    mutate(n_obs = tidyr::replace_na(n, replace = 0)) %>%
    mutate(prob = replace(prob, bin == n_bins, 1-sum(prob[1:(n_bins - 1)]))) %>%
    select(bin, n_obs, prob)
  
  tmp3 <- chisq.test(x = chi_sq_data$n_obs, p = chi_sq_data$prob, simulate.p.value = T)
  
  out <- list(
    summary = tibble(
      n_bins = n_bins, 
      X_squared = tmp3$statistic, 
      p = tmp3$p.value, 
      lambda = lambda),
    exp_obs = tibble(
      bin = chi_sq_data$bin,
      count = 0:(n_bins - 1),
      observed = tmp3$observed,
      expected = tmp3$expected,
      residuals = tmp3$residuals
    )
  )
  return(out)
  
}
