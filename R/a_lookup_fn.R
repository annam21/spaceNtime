#' Make lookup table for camera area
#'
#' This function will create a summary table of all cameras and their areas.
#' NOTE! Estimates of abundance will be wrong if any cameras are missing from x (i.e., the camera
#'   was functioning but didn't get any pictures.)
#'
#' Disclaimer! This function currently does not account for camera areas that change over time.
#'
#' @param x Camera database with columns cam and a (camera area)
#'
#' @return A dataframe with one row per camera, and its area
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' df <- data.frame(cam = c(1,1,2,2),
#'                  datetime = as.POSIXct(c("2016-01-02 12:00:00",
#'                                          "2016-01-03 13:12:00",
#'                                          "2016-01-02 14:00:00",
#'                                          "2016-01-03 16:53:42"),
#'                                        tz = "GMT" ),
#'                  a = c(850, 850, 1100, 1100),
#'                  count = c(1, 0, 0, 2))
#' a_lookup_fn(df)
a_lookup_fn <- function(x) {
  x %>%
    dplyr::group_by(cam) %>%
    dplyr::summarise(a = dplyr::first(a))
}
