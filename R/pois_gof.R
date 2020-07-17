# Poisson_GOF_Test
# Kenneth Loonam
# June 20202

#Variables======================================================================

lambda <- 0.001
n_occ <- 10000
n_bins <- 3

#Environment====================================================================

require(dplyr); require(spaceNtime)

data <- tibble(count = rpois(n_occ, lambda))

#Functions======================================================================

pois_dens <- function(x, lambda){
  out <- lambda^x * exp(-lambda) / factorial(x)
}

#Workflow=======================================================================

data_gof <- data %>%
  mutate(bin = count + 1) %>%
  mutate(bin = replace(bin, bin > n_bins, n_bins)) %>%
  group_by(bin) %>%
  tally() 
  # mutate(cutoff = bin - 1) %>%
  # mutate(prob = pois_dens(cutoff, lambda)) %>%
  # mutate(prob = replace(prob, bin == n_bins, ppois(max(cutoff) - 1, lambda, lower.tail = F)))

data_chsq <- tibble(
  bin = 1:n_bins, 
  prob = pois_dens(bin - 1, lambda)
  ) %>%
  full_join(data_gof, by = "bin") %>%
  mutate(n_obs = tidyr::replace_na(n, replace = 0)) %>%
  mutate(prob = replace(prob, bin == n_bins, 1-sum(prob[1:(n_bins - 1)]))) %>%
  select(bin, n_obs, prob)

chisq.test(x = data_chsq$n_obs, p = data_chsq$prob, simulate.p.value = T)
