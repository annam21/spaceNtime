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
  t <- occ %>% 
    mutate(d = as.numeric(difftime(start, lag(start), units = "secs")),
           m = min(d, na.rm = T),
           r = d %% m)
  
  if(any(t$r > 0, na.rm = T)){
    warning("Occasions are not evenly spaced, so ste_calc_toevent must go the slow 
            route")
    # Find sampling occasions where counts exist
    # This captures any count within the occasion. Later, I take only the first
    count_at_occ <- df %>%
      filter(count > 0) %>%
      left_join(effort, .,  by = "cam") %>%
      filter(datetime %within% int) %>%
      select(occ, cam, count)
    
  } else {
    
    # Do it with rounding instead of intervals!!! 
    ss <- t$m[1]
    count_at_occ <- df %>%
      filter(count > 0) %>%
      # round down to the nearest interval
      mutate(timefromfirst = as.numeric(datetime) - as.numeric(min(effort$start)),
             nearest = plyr::round_any(timefromfirst, ss, f = floor),
             nearestpos = as.POSIXct(nearest, 
                                     origin = min(effort$start), 
                                     tz = lubridate::tz(effort$start))
      ) %>%
      left_join(effort, ., by = c("start" = "nearestpos", "cam")) %>%
      select(occ, cam, count) 
  }
  
  ### begin practice 
 
 
  # See if I can do this in two steps to make it faster...
  # t1 <- df %>%
  #   filter(count > 0) %>%
  #   mutate(date = as.Date(datetime))
  # # filter(date == as.Date("2016-01-22"))
  # # distinct(cam, count, date, .keep_all = T)
  # t2 <- effort %>%
  #   mutate(date = as.Date(start))
  # # distinct(cam, area, date, .keep_all = T)
  # t3 <- left_join(t1, t2, by = c("cam", "date")) %>%
  #   filter(datetime %within% int)
  # 
  # t3 <- left_join(t2, t1, by = c("cam", "date") ) %>%
  #   filter(datetime %within% int) %>%
  #   select(occ, cam, count).
  # NO! we just need to get the occ number for each photo!!!!
  # # THEN we add back the camera number.
  # t1 <- df %>%
  #   filter(count > 0) %>%
  #   distinct(datetime) %>%
  #   mutate(date = as.Date(datetime))
  # # filter(date < as.Date("2016-01-02"))
  # t2 <- effort %>%
  #   select(-cam, -area) %>%
  #   distinct(int, .keep_all = T) %>%
  #   mutate(date = as.Date(start))
  # t3 <- full_join(t1, t2, by = "date") %>% 
  #   filter(datetime %within% int)
  
  # What I want to do is:
  # Find out if the picture is in ANY occasion. Nope. 
  # for(i in 1:nrow(t1)){
  #   t1$anyin[i] <- any(t1$datetime[i] %within% t2$int)
  # }
  # 

  #end practice

  tmp <- effort %>%
    # Randomly order cameras at each occasion
    group_by(occ) %>% 
    sample_n(n()) %>%
    
    # Join up our counts
    left_join(., count_at_occ, by = c("occ", "cam")) %>% 
    # Here NAs are pictures that didn't exist. 0s are counts of 0 
    # count should be NA if area = 0. 
    
    # Find the area until the first count, at each occcasion
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
