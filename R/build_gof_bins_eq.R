build_gof_bins_eq <- function(eh, n_bins, lambda){
  # function to build bins for goodness of fit test with bin cutoffs the same
    # for every occasion
  # bin probabilities vary between occasions when (cesnor < max(censor))
  
  # highest observed censor value, used to set bins of = width
  mx_cen <- max(eh$censor) 
  
  # define cutoff values between bins
  cutoffs <- seq(0, mx_cen, length.out = n_bins)
  
  bin_data_raw <- rep(
    # initial bins, one df per occasion, bins adjusted later
    list(
      tibble(
        bin = 1:n_bins, 
        left = cutoffs, 
        right = lead(cutoffs)
      )
    ), nrow(eh)
  ) %>%
    # set the max right cutoff for occasion[i] to the censor value[i] 
    map2(.,
      as.list(eh$censor), 
      function(a,b){ 
        out <- a %>%
          mutate(right = replace(right, right > b, b))
        return(out)
      }
    ) %>%
    # assign probabilities to each bin, each occasion, given lambda
    map(.,
      mutate,
      prob = exp_dens(left, right, lambda)
    ) %>%
    # set neg probs to 0 (adjust for censor value moving between occasions)
    map(
      .,
      mutate,
      prob = replace(prob, prob < 0, 0)
    ) %>%
    # set final bin probability to 1 - sum of probabilities (i.e. the rest of the curve)
    map(
      .,
      mutate,
      prob = tidyr::replace_na(prob, replace = 1-sum(prob, na.rm = T))
    )

  return(bin_data_raw)
}
