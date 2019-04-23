#' Determine effort at each camera on each occasion
#'
#' @param deploy deploy object
#' @param occ occasions dataframe or tibble
#'
#' @return a dataframe or tibble the with the area at each camera and occasion 
#' @details If any occasion is missing from deploy for a camera, it is assumed 
#' that the camera was not working at that time. 
#' @export
#' @importFrom lubridate "%within%"
#'
#' @examples effort_fn(deploy, occ)
effort_fn <- function(deploy, occ){
  # Create intervals in deploy
  deploy <- deploy %>%
    add_int(.) 

  # Create occasions by camera
  occ_by_cam <- build_occ_cam(deploy, occ) %>%
    add_int(.)
  
  # Combine occ-by-cam and deploy to get area-by-occ-by-cam
  effort <- occ_by_cam %>%
    rename(occ_int = int) %>% 
    left_join(., deploy, by = "cam") %>%
    filter(occ_int %within% int) %>%
    select(occ, cam, area) %>% 
    left_join(occ_by_cam, ., by = c("occ", "cam")) %>% 
    mutate(area = replace(area, is.na(area), 0))
  return(effort)
}