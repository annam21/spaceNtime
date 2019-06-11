#' Estimate N through ISE
#'
#' @param ise_eh an instantaneous sampling encounter history
#' @param study_area the size of the study area (same units as camera area)
#'
#' @return a tibble with density and abundance estimate
#' @export
#'
#' @examples 
#' ise_eh <- ise_build_eh(df, deploy, occ) 
#' ise_estN_fn(ise_eh, 1e6)
ise_estN_fn <- function(ise_eh, study_area){

  # First, get rid of occasions where area = 0
  ise_eh2 <- ise_eh %>%
    filter(area != 0)
  
  Jai_ni <- ise_eh2 %>%
    group_by(cam) %>%
    summarise(Jai = sum(area),
              ni = sum(count))
  
  n_L <- Jai_ni %>%
    summarise(n = sum(ni),
              L = sum(Jai))
  
  M <- length(unique(Jai_ni$cam))
  L <- n_L$L
  n <- n_L$n
  Jai <- Jai_ni$Jai
  ni <- Jai_ni$ni
  
  varD <- M / L^2 / (M-1) * sum(Jai^2  * (ni/Jai - n/L)^2)
  form <- sprintf("~ %f * x1", study_area)
  
  ise_est <- ise_eh2 %>%
    mutate(dens_ij = count/area) %>%
    summarise(D = mean(dens_ij) ) %>%
    mutate(N = D * study_area,
           varD = varD,
           SE_N = msm::deltamethod(as.formula(form), D, varD))
  
  CI <- logCI(ise_est$N, ise_est$SE_N)
  
  out <- ise_est %>%
    select(N, SE_N) %>%
    bind_cols(CI)
    
  return(out)
}