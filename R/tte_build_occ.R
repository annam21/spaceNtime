#' tte_build_occ
#'
#' @param per_length numeric. Length of the TTE sampling period.
#' @param nper numeric. Number of TTE sampling periods per sampling occasion.
#' @param time_btw numeric. Length of time between sampling occasions, 
#' allowing animals to re-randomize.
#' @param study_start POSIXct. The start of the study. 
#' @param study_end POSIXct. The end of the study.
#' 
#' @return a dataframe
#' @export
#'
#' @examples
#' deploy <- data.frame(
#'   cam = c(1, 2, 2, 2),
#'   start = as.POSIXct(c("2015-12-01 15:00:00",
#'                        "2015-12-08 00:00:00", 
#'                        "2016-01-01 00:00:00", 
#'                        "2016-01-02 00:00:00"),
#'                      tz = "GMT"),
#'   end = as.POSIXct(c("2016-01-05 00:00:00", 
#'                      "2015-12-19 03:30:00", 
#'                      "2016-01-01 05:00:00",
#'                      "2016-01-05 00:00:00"), 
#'                    tz = "GMT"),
#'   area = c(300, 200, 200, 450)
#' )
#' per <- tte_samp_per(deploy, lps = 30/3600)
#' study_dates <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")
#' occ2 <- tte_build_occ(
#'   per_length = per,
#'   nper = 24,
#'   time_btw = 2 * 3600,
#'   study_start = study_dates[1],
#'   study_end = study_dates[2]
#' )
tte_build_occ <- function(per_length, nper, time_btw, study_start, study_end){
  # Occasions without camera
  # (study_end - study_start)/per_length * nper
  occ_length <- per_length * nper
  samp_freq <- occ_length + time_btw
  
  data.frame(
    start = sampling_start(samp_freq = samp_freq,
                           date_lim = c(study_start, study_end))
  ) %>%
    mutate(end = start + occ_length,
           occ = 1:n()) %>%
    select(occ, start, end)
}



