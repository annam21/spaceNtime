#' Create STE encounter history
#'
#' User defines sampling period and frequency. This function randomizes active
#' cameras at every time step and finds the STE. Then it formats it as the input
#'  to ste_estN_fn
#'
#' Disclaimer! This function currently does not account for cameras that are not active
#' during the entire sampling period.
#' Disclaimer! I'm not sure what it will do with NAs in the count column yet.
#'
#' @param x A dataframe with columns named cam (any class) and datetime (class POSIXct)
#' and a column (any name) of the count of target species in each photo.
#' @param count_col The name of the column containing the count of animals
#' @param samp_freq How often to sample, in seconds. E.g., samp_freq = 3600 for 1 sampling occasion every hour
#' @param samp_length The desired length of each sampling occasion, in seconds. E.g., 
#' samp_length = 10 for sampling occasions that last 10 seconds
#' @param cam_areas A dataframe made by a_lookup_fn or similar (with columns cam and a).
#'  Must have one row per active camera and that camera's area (a)
#' @param date_lim A vector of length 2 of class POSIXct. The first and last date of the
#' desired sampling period.
#' @param A The size of the study area (same units as a)
#'
#' @return A list with the encounter history for an STE model.
#' @export
#' @importFrom magrittr "%>%" 
#'
#' @examples
#' df <- data.frame(cam = c(1,1,2,2),
#'             datetime = as.POSIXct(c("2016-01-02 12:00:00",
#'                                     "2016-01-03 13:12:00",
#'                                     "2016-01-02 14:00:00",
#'                                     "2016-01-03 16:53:42"),
#'                                    tz = "GMT"),
#'                   a = c(850, 850, 1100, 1100),
#'                   count = c(1, 0, 0, 2))
#' tab <- a_lookup_fn(df)
#' d <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")
#' ste_data_fn(df,
#'             count_col = "count",
#'             samp_freq = 3600,
#'             samp_length = 10,
#'             cam_areas = tab,
#'             date_lim = d,
#'             A = 150000)
ste_data_fn <- function(x, count_col, samp_freq, samp_length, cam_areas, date_lim, A){

  # Make sure time zones match
  stopifnot(lubridate::tz(date_lim) == lubridate::tz(x$datetime))
  
  # Create a vector of sampling start times
  st <- sampling_start(samp_freq = samp_freq,
                       date_lim = date_lim)
  
  # Find pictures WITH animals that are actually in a sampling period
  tmp <- x %>%
    dplyr::mutate(timer = diff_fn(datetime, st, interval_length = samp_length),
                  timer = as.POSIXct(timer, origin = "1970-01-01 00:00:00",
                                     tz = lubridate::tz(x$datetime)) ) %>%
    dplyr::filter(!is.na(timer),
                  .[, which(names(.) == count_col)] > 0 ) %>% # where count > 0
    dplyr::mutate(event = as.numeric(as.factor(.$timer))) # Give each event a number
  
  # For events WITH a picture, find the space-to-event
  out <- data.frame(datetime = as.POSIXct(NA), 
                    areatoevent = NA)
  for(i in 1:length(unique(tmp$event))){
    cc <- sample(cam_areas$cam, length(cam_areas$cam), replace = F)
    tmp2 <- tmp %>%
      dplyr::filter(event == i) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(camtoevent = which(cc == cam)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(event) %>%
      dplyr::summarise(camtoevent = min(camtoevent),
                       datetime = dplyr::first(datetime)) %>%
      dplyr::mutate(areatoevent = sum(cam_areas$a[1:camtoevent]) ) %>%
      dplyr::select(datetime, areatoevent) 
    out[i,] <- tmp2
  }
  
  # For events WITHOUT a picture, add in an NA
  idf <- data.frame("datetime" = st)
  toevent <- idf %>%
    dplyr::anti_join(., tmp, by = c("datetime" = "timer")) %>%
    dplyr::mutate(toevent = NA) %>%
    dplyr::bind_rows(., out) %>% 
    dplyr::arrange(datetime) %>% 
    tidyr::spread(datetime, areatoevent) %>%
    dplyr::select(-toevent) %>%
    as.matrix()
  
  dat.ste <- list(toevent = toevent,
                  censor = sum(cam_areas$a),
                  A = A
  )

  return(dat.ste)
}
