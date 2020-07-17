# Poisson_GOF_Test
# Kenneth Loonam
# June 20202
# potentially only space-to-event???

#Variables======================================================================

lambda <- 0.00001
n_occ <- 10000
n_bins <- 10
censor <- 500

#Environment====================================================================

require(dplyr); require(spaceNtime); require(purrr)

data <- tibble(to_event = rexp(n_occ, lambda)) %>%
  mutate(to_event = replace(to_event, to_event > censor, NA))

#Functions======================================================================

exp_dens <- function(left, right, lambda){
  pleft <- pexp(left, lambda); pright <- pexp(right, lambda)
  out <- pright - pleft
  return(out)
}

build_bins <- function(n_bins, censor, lambda){
  cutoffs <- seq(0, censor, length.out = n_bins)
  bin_data <- tibble(
    bin = 1:n_bins, 
    left = cutoffs, 
    right = lead(cutoffs),
    prob = exp_dens(left, right, lambda)
    ) %>%
    mutate(prob = tidyr::replace_na(prob, replace = 1-sum(prob, na.rm = T)))
}

assign_bins <- function(x, bin_data, to_event){
  out <- between(to_event, bin_data$left[x], bin_data$right[x])
}

#Workflow=======================================================================

bin_data <- build_bins(n_bins, censor, lambda)

x <- 1:n_bins
names(x) <- as.character(1:n_bins)

bin_counts <- map_dfr(x, assign_bins, bin_data, data$to_event) %>%
  apply(2, sum, na.rm = T)

data_chsq <- bin_data %>%
  mutate(n_obs = bin_counts) %>%
  mutate(n_obs = replace(n_obs, bin == n_bins, n_occ - sum(n_obs)))

chisq.test(x = data_chsq$n_obs, p = data_chsq$prob, simulate.p.value = T)
