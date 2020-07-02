#' Determine effort at each camera on each occasion
#'
#' @param deploy deploy object
#' @param occ occasions dataframe or tibble
#'
#' @return a dataframe or tibble the with the area at each camera and occasion 
#' @details If any occasion is missing from deploy for a camera, it is assumed 
#' that the camera was not working at that time. 
#' @export
#'
#' @examples effort_fn(deploy, occ)
effort_fn <- function(deploy, occ){
  # Create intervals in deploy
  deploy <- deploy %>%
    add_int(.) 

  # Create occasions by camera
  occ_by_cam <- build_occ_cam(deploy, occ) %>%
    add_int(.)
  
  # Try interval overlap to combine the two
  effort <- occ_by_cam %>% 
    rename(occ_int = int) %>% #.02s
    left_join(., deploy, by = "cam") %>% # 2s
    filter(lubridate::int_overlaps(occ_int, int) ) %>% #5s
    select(occ, cam, area) %>% # 5s
    left_join(occ_by_cam, ., by = c("occ", "cam")) %>% #18s #### big time suck
    mutate(area = replace(area, is.na(area), 0)) %>% #19s
    
    # get rid of duplicate rows (if area changed during occasion)
    distinct(occ, cam, .keep_all = T) %>% #23s
    ungroup()

  return(effort)
}
