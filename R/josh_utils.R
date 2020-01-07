snt_effort_alt <- function(df, stdy_start, stdy_end, cam_min = 0){

  # Only retain those values falling within one of the occ's above
  # Label the images with occ using modulo division
  out <- df %>%
    # Subset to study period
    filter(between(datetime, stdy_start, stdy_end)) %>%
    group_by(datetime) %>%
    filter(sum(area) > 0) %>%
    mutate(
      occ = datetime,
      ncam = n_distinct(cam)
    ) %>%
    filter(ncam > cam_min) %>%
    ungroup() %>%
    arrange(occ) %>%
    mutate(
      occ = as.integer(as.factor(datetime))
    )
}

ste_calc_toevent_alt <- function(eff){
  
  out <- eff %>% 
    group_by(occ) %>% 
    mutate(
      STE = cumsum(area)
    ) %>% 
    summarise(
      censor = max(STE),
      STE = STE[count > 0][1]
    ) %>%
    ungroup()
  
  if (all(is.na(out$STE))) {
    warning("No animals detected in any sampling occasion")  
  }
  
  return(out)
}
