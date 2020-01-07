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
#'  calc_ste(df, occ, eff)
ste_calc_toevent <- function(df, occ, effort){
  # If deploy said the camera was on and there is no photo, I'm assuming a count of 0. 
  
  # Find sampling occasions where counts exist
  # This captures any count within the occasion. Later, I get rid of 0s and 
  # take only the first
  count_at_occ <- df %>%
    filter(count > 0) %>%
    left_join(effort, .,  by = "cam") %>% 
    filter(datetime %within% int) %>%
    select(occ, cam, count)
  
  tmp <- effort %>%
    # Randomly order cameras at each occasion
    group_by(occ) %>% 
    #sample_n(n()) %>%
    
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