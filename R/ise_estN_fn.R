#' Estimate N through ISE
#'
#' @param ise_eh an instantaneous sampling encounter history
#' @param study_area the size of the study area (same units as camera area)
#'
#' @return a tibble with density and abundance estimate
#' @export
#'
#' @examples
is_estN_fn <- function(ise_eh, study_area){
  
  ise_eh %>%
    mutate(dens_ij = count/area) %>%
    summarise(D = mean(dens_ij, na.rm = T) ) %>%
    mutate(N = D * study_area)
  
}