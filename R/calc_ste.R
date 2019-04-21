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
calc_ste <- function(df, occ, effort){
  tmp <- df %>%
    # Find count at each sampling occasion
    left_join(effort, .,  by = "cam") %>% 
    filter(datetime %within% int) %>% 
    
    # Randomly order the cameras at each occasion
    filter(area != 0) %>% 
    group_by(occ) %>% 
    sample_n(n()) %>%
    
    # Find the area until the first count, at each occcasion
    mutate(STE = cumsum(area)) %>% 
    filter(count > 0) %>% 
    filter(!duplicated(occ)) %>% 
    
    # Add back NAs on all other sampling occasions
    select(occ, STE) %>%
    left_join(occ, ., by = "occ")
  
  if(all(is.na(tmp$STE))) warning("No animals detected in any sampling occasion")

  return(tmp)
}