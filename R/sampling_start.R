#' Create sequence of start times
#'
#' Create a sequence of start times by defining the amount of time between each, in seconds.
#' E.g., samp_freq = 3600 for sampling once per hour.
#'
#' @param samp_freq The number of seconds between each sampling period
#' @param date_lim A vector of length 2 of class POSIXct. The first and last date of the
#' desired sampling period.
#'
#'
#' @return A sequence of dates
#' @export
#'
#' @examples
#' d <- c(Sys.time()-43200, Sys.time())
#' sampling_start(3600, d)
#'
sampling_start <- function(samp_freq, date_lim) {

  # Make sure date_lim is the right dimension and class
  stopifnot(is.null(date_lim) | length(date_lim) == 2)
  stopifnot(class(date_lim) == c("POSIXct", "POSIXt"))

  # Vector of all the start times
  s <- seq(from = date_lim[1], to = date_lim[2], by = paste(samp_freq, "sec"))

  # Take out the last time in s, because it will almost never be a full sampling period.
  s <- s[1:(length(s) - 1)]

  # Output
  return(s)
}
