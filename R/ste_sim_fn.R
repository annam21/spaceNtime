#' STE simulation for one combination of values
#'
#' @param N a single value, true abundance for simulation
#' @param study_area a single value, true study area size (N/study_area = true density)
#' @param ncam a single value, number of cameras to simulate
#' @param nocc a single value, number of occasions to simulate
#' @param cam_area a single value, size of the camera viewshed
#'
#' @return a tibble
#' @export
#'
#' @examples
#' ste_pwr_sim(
#'   N = c(5, 15)
#'   study_area = 1e6,
#'   ncam = c(50, 100),
#'   nocc = c(10000, 20000),
#'   cam_area = 150,
#'   niter = 30
#' )
ste_sim_fn <- function(N, study_area, ncam, nocc, cam_area, pb){
  # Progress bar
  pb$tick()$print() # deprecated dplyr version
  # pb$tick() # New version
  
  # Derived variables
  lambda <- N / study_area
  nocc <- nocc
  censor <- ncam * cam_area
  
  to_event <- rexp(n = nocc, rate = lambda) %>%
    replace(., . > censor, NA)
  
  if(all(is.na(to_event))){
    out <- tibble(
      TrueN = N,
      StudyArea = study_area,
      NCam = ncam,
      NOcc = nocc,
      CamArea = cam_area,
      EstN = NA,
      SE = NA,
      LCI = NA,
      UCI = NA
    )
  }else{
    eh <- list(STE = to_event, censor = rep(censor, length(to_event)))
    tmp <- ste_estN_fn(eh, study_area)
    out <- tibble(
      TrueN = N,
      StudyArea = study_area,
      NCam = ncam,
      NOcc = nocc,
      CamArea = cam_area,
      EstN = tmp$N,
      SE = tmp$SE,
      LCI = tmp$LCI,
      UCI = tmp$UCI
    )
  }
  return(out)
}
