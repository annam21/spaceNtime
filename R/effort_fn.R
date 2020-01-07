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
    rename(occ_int = int) %>%
    left_join(deploy, by = "cam") %>%
    filter(lubridate::int_overlaps(occ_int, int) ) %>% 
    select(occ, cam, area) %>%
    left_join(occ_by_cam, ., by = c("occ", "cam")) %>% 
    mutate(area = replace(area, is.na(area), 0)) %>%
    
    # get rid of duplicate rows (if area changed during occasion)
    group_by(occ, cam, start, end, int) %>%
    summarise(area = first(area)) %>%
    ungroup()

  return(effort)
}