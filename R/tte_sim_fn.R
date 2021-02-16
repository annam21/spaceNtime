#' TTE simulation for one combination of values
#'
#' @param N a single value, true abundance for simulation
#' @param study_area a single value, true study area size (N/study_area = true density)
#' @param ncam a single value, number of cameras to simulate
#' @param nocc a single value, number of occasions to simulate
#' @param nper a numeric vector, number of sampling periods per sampling occasion
#' @param cam_area a single value, size of the camera viewshed
#' @param pb progress bar
#'
#' @return a tibble
#' @export
#'
#' @examples
#' tte_pwr_sim(
#'   N = c(5, 15),
#'   study_area = 1e6,
#'   ncam = c(50, 100),
#'   nocc = c(10000, 20000),
#'   nper = 20
#'   cam_area = 150,
#'   niter = 30
#' )
tte_sim_fn <- function(N, study_area, ncam, nocc, nper, cam_area, pb){
  # Progress bar
  pb$tick()$print() # deprecated dplyr version
  # pb$tick() # New version
  
  # Derived variables
  lambda <- N / study_area
  censor <- nper
  
  to_event <- rexp(n = nocc*ncam, rate = lambda) %>%
    replace(., . > censor, NA) %>% 
    as.matrix(nrow = ncam)
  
  if(all(is.na(to_event))){
    out <- tibble(
      TrueN = N,
      StudyArea = study_area,
      NCam = ncam,
      NOcc = nocc,
      NPer = nper,
      CamArea = cam_area,
      EstN = NA,
      SE = NA,
      LCI = NA,
      UCI = NA
    )
  }else{
    eh <- list(TTE = to_event, 
               censor = rep(censor, length(to_event)),
               cam = 1:ncam)
    tmp <- tte_estN_fn(eh, study_area)
    out <- tibble(
      TrueN = N,
      StudyArea = study_area,
      NCam = ncam,
      NOcc = nocc,
      NPer = nper,
      CamArea = cam_area,
      EstN = tmp$N,
      SE = tmp$SE,
      LCI = tmp$LCI,
      UCI = tmp$UCI
    )
  }
  return(out)
}
