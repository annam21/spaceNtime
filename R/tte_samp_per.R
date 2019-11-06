#' Sampling period length
#'
#' @param deploy deploy object
#' @param lps movement speed in length units (same as camera area measurement) per second
#'
#' @return The average length of time, in seconds, needed to cross a camera viewshed
#' @export
#' @section Warning:
#' This function only calculates a rough estimate of the time needed to move across a camera. 
#' There may be better ways. 
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
#' samp_per(deploy, lps = 2)
tte_samp_per <- function(deploy, lps){
  mn_area <- mean(deploy$area)
  # Super rough estimate of distance across
  out <- sqrt(mn_area)/lps
  return(out)
}
