#' Calculate STE on each occasion
#'
#' @param df df object
#' @param occ occasions
#' @param effort effort from effort_fn()
#'
#' @return a dataframe with the same rows as occ, with a STE column
#' @export
#' @importFrom lubridate "%within%"
#'
#' @examples
#'   occ <- build_occ(samp_freq = 3600, 
#'             samp_length = 10,
#'             study_start = study_dates[1],
#'             study_end = study_dates[2])
#'  eff <- effort_fn(deploy, occ)
#'  ste_calc_toevent(df, occ, eff)
ste_calc_toevent <- function(df, occ, effort){
  # If deploy said the camera was on and there is no photo, I'm assuming a count of 0. 
  
  # THIS IS A PATCH THAT CAN BE CLEANED UP
  # I'm including the fast and slow way to find which occasion a picture is in
  
  # Calculate the lowest common denominator for samp_freq 
  ss <- lcd_fn(occ)
  
  if(ss == 0){
    warning("Occasions are not evenly spaced, so ste_calc_toevent must go the slow 
            route")
    # Find sampling occasions where counts exist
    # This captures any count within the occasion. Later, I take only the first
    count_at_occ <- df %>%
      filter(count > 0) %>%
      left_join(effort, .,  by = "cam") %>%
      filter(datetime %within% int) %>%
      # Take only the first at each camera at each occasion
      distinct(occ, cam, .keep_all = T) %>%
      select(occ, cam, count)
    
  } else {
    
    # Do it with rounding instead of intervals!!! 
    count_at_occ <- df %>%
      filter(count > 0) %>%
      # round down to the nearest interval
      mutate(timefromfirst = as.numeric(datetime) - as.numeric(min(effort$start)),
             nearest = plyr::round_any(timefromfirst, ss, f = floor),
             nearestpos = as.POSIXct(nearest, 
                                     origin = min(effort$start), 
                                     tz = lubridate::tz(effort$start))
      ) %>%
      # Join by the start time of that interval
      left_join(., effort, by = c("nearestpos" = "start", "cam")) %>%
      # But then keep only if within the end date of that interval
      filter(datetime <= end) %>% 
      select(occ, cam, count) %>%
      # Only keep the first one at each camera on each occasion
      distinct(occ, cam, .keep_all = T)
  }
  
  tmp <- effort %>%
    # Randomly order cameras at each occasion
    group_by(occ) %>% 
    sample_n(n()) %>%
    
    # Join up our counts
    left_join(., count_at_occ, by = c("occ", "cam")) %>% 
    # Here NAs are pictures that didn't exist. 0s are counts of 0 
    # count should be NA if area = 0. 
    
    # Find the area until the first count, at each occasion
    mutate(STE = cumsum(area)) %>% 
    # If the area is 0, it's just adding 0 to the STE. That's good. 
    filter(count > 0) %>% 
    # Here I'm filtering NAs too, so if a photo doesn't exist, its count is 0 
    filter(!duplicated(occ)) %>% 
    
    # Add back NAs on all other sampling occasions
    select(occ, STE) %>%
    left_join(occ, ., by = "occ")
  
  if(all(is.na(tmp$STE))) warning("No animals detected in any sampling occasion")

  return(tmp)
}
