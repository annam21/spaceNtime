tte_pwr_sim <- function(N, study_area, ncam, nocc, cam_area, speed, nper, niter){
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
  
  out <- purrr::pmap_dfr(.l = params, 
                         .f = tte_sim_fn,  
                         nper = nper, 
                         pb = pb)
  
  return(out)
}
