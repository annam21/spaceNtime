snt_effort_alt <- function(deploy = NULL, df, stdy_start, stdy_end, samp_freq, samp_length){

  # Create occasions
  occ <- build_occ(
      samp_freq, # seconds between the start of each sampling occasion
      samp_length, # duration of each sampling occasion (seconds)
      study_start = stdy_start,
      study_end = stdy_end
    ) %>%
    add_int() %>%
    select(occ, int)
  
  # Only retain those values falling within one of the occ's above
  # Label the images with occ using modulo division
  out <- df %>%
    filter(between(datetime, stdy_start, stdy_end)) %>%
    filter(datetime %within% as.list(occ$int)) %>%
    mutate(
      int = lubridate::interval(stdy_start, datetime),
      occ = int %/% lubridate::seconds(samp_freq) + 1
    )

}

ste_calc_toevent_alt <- function(eff){
  # If deploy said the camera was on and there is no photo, I'm assuming a count of 0. 
  
  # Find sampling occasions where counts exist
  # This captures any count within the occasion. Later, I get rid of 0s and 
  # take only the first
  # count_at_occ <- eff %>%
  #   filter(count > 0) %>%
  #   select(occ, cam, count)
  
  tmp <- eff %>%
    # Randomly order cameras at each occasion
    group_by(occ) %>% 
    sample_n(n()) %>%
    # Here NAs are pictures that didn't exist. 0s are counts of 0 
    # count should be NA if area = 0. 
    # Find the area until the first count, at each occcasion
    mutate(STE = cumsum(area)) %>% 
    # If the area is 0, it's just adding 0 to the STE. That's good. 
    filter(count > 0) %>% 
    # Here I'm filtering NAs too, so if a photo doesn't exist, its count is 0 
    #filter(!duplicated(occ)) %>% 
    # Because the data are grouped and we want the first I would rather rely on
    #  the date here
    filter(datetime == min(datetime)) %>%
    ungroup() %>%
    # Add back NAs on all other sampling occasions
    select(occ, STE) %>%
    #left_join(occ, ., by = "occ")
    complete(occ = seq(1, max(occ), by = 1))
  
  if(all(is.na(tmp$STE))) warning("No animals detected in any sampling occasion")
  
  return(tmp)
}
