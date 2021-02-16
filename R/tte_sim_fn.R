tte_sim_fn <- function(N, study_area, ncam, nocc, cam_area, nper, pb){
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
      CamArea = cam_area,
      EstN = tmp$N,
      SE = tmp$SE,
      LCI = tmp$LCI,
      UCI = tmp$UCI
    )
  }
  return(out)
}
