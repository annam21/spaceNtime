#' Build sampling occasions
#'
#' @param samp_freq numeric, The number of seconds between the start of each sampling occasion
#' @param samp_length numeric. The number of seconds to sample at each sampling occasion
#' @param study_start POSIXct. The start of the study 
#' @param study_end POSIXct. The end of the study
#'
#' @return a dataframe 
#' @export 
#'
#' @examples 
#'   study_dates <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")
#'   build_occ(samp_freq = 3600, 
#'             samp_length = 10,
#'             study_start = study_dates[1],
#'             study_end = study_dates[2])
build_occ <- function(samp_freq, samp_length, study_start, study_end){
  # Occasions without camera
  data.frame(
    start = sampling_start(samp_freq = samp_freq,
                           date_lim = c(study_start, study_end))
  ) %>%  
    mutate(end = start + samp_length,
           occ = 1:n()) %>% 
    select(occ, start, end) 
}

#' Sampling occasions by camera
#' 
#'
#' @param deploy deploy object
#' @param occ dataframe of sampling occasions
#'
#' @return dataframe
#' @export
#'
#' @examples
#'   study_dates <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")
#'   occ <- build_occ(samp_freq = 3600, 
#'             samp_length = 10,
#'             study_start = study_dates[1],
#'             study_end = study_dates[2])
#'   build_occ_cam(deploy, occ)
build_occ_cam <- function(deploy, occ){
  tidyr::crossing(
    occ = occ$occ, 
    cam = deploy$cam
  ) %>% 
    dplyr::left_join(., occ, by = "occ") 
}