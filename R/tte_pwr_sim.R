#' Simulation-based TTE power analysis
#'
#' @param N a numeric vector, true abundance for simulation
#' @param study_area a numeric vector, true study area size (N/study_area = true density)
#' @param ncam a numeric vector, number of cameras to simulate
#' @param nocc a numeric vector, number of occasions to simulate
#' @param nper a numeric vector, number of sampling periods per sampling occasion
#' @param cam_area  a numeric vector, size of the camera viewshed
#' @param niter a single value, number of simulation iterations for each N x 
#' study_area x ncam x nocc combination
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
tte_pwr_sim <- function(N, study_area, ncam, nocc, nper, cam_area, niter){
  # Data checks
  stopifnot(is.numeric(N))
  stopifnot(is.numeric(study_area))
  stopifnot(is.numeric(ncam))
  stopifnot(is.numeric(nocc))
  stopifnot(is.numeric(cam_area))
  stopifnot(length(niter)==1)
  
  data_list <- list(
    N = N, 
    study_area = study_area, 
    ncam = ncam, 
    nocc = nocc, 
    cam_area = cam_area,
    nper = nper
  )
  
  params <- expand.grid(data_list) %>% 
    slice(rep(1:n(), each = niter))
  
  # Progress bar
  pb <- dplyr::progress_estimated(nrow(params)) # deprecated dplyr version
  # pb <- progress::progress_bar$new() # This should be the newer version
  
  out <- purrr::pmap_dfr(.l = params, 
                         .f = tte_sim_fn, 
                         pb = pb)
  
  return(out)
}
