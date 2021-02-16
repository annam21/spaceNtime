#' Simulation-based STE power analysis
#'
#' @param N a numeric vector, true abundance for simulation
#' @param study_area a numeric vector, true study area size (N/study_area = true density)
#' @param ncam a numeric vector, number of cameras to simulate
#' @param nocc a numeric vector, number of occasions to simulate
#' @param cam_area a numeric vector, size of the camera viewshed
#' @param niter a single value, number of simulation iterations for each N x 
#' study_area x ncam x nocc combination
#'
#' @return a tibble
#' @export
#'
#' @examples
#' ste_pwr_sim(
#'   N = c(5, 15),
#'   study_area = 1e6,
#'   ncam = c(50, 100),
#'   nocc = c(10000, 20000),
#'   cam_area = 150,
#'   niter = 30
#' )
ste_pwr_sim <- function(N, study_area, ncam, nocc, cam_area, niter){
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
    cam_area = cam_area
  )

  params <- expand.grid(data_list) %>% 
    slice(rep(1:n(), each = niter))

  # Progress bar
  pb <- dplyr::progress_estimated(nrow(params)) # deprecated dplyr version
  # pb <- progress::progress_bar$new() # This should be the newer version
  
  out <- purrr::pmap_dfr(.l = params, .f = ste_sim_fn, pb)

  return(out)
}

# Try running in parallel with furrr instead of purrr 