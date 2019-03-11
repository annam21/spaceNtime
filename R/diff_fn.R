#' Select rows within an interval
#'
#' Used within mutate, this function creates a new column with the first value of y
#' where x-y is less than or equal to a specified interval.
#'
#'
#' @param x A numeric vector or the name of a column in a dataframe
#' @param y A numeric vector of the same length
#' @param interval_length The interval between y and x
#'
#' @return A vector of the same length as x, with the first value of y where x-y <= interval_length
#' @export
#'
#' @examples
#' df <- data.frame(ID = 1:6, datetime = c(1, 2, 5, 6, 10, 8))
#' y <- c(2,4,6,8)
#' diff_fn(df, y, 1)
#'
diff_fn <- function(x, y, interval_length) {
  out <- x %>%
    #  Throw out images taken before or after interval of interest
    dplyr::filter(dplyr::between(datetime, min(y), max(y))) %>%
    dplyr::mutate(
      #  Find corresponding interval for each image
      Bin = findInterval(datetime, y),
      #  Replace no match value (0) with NA
      Bin = replace(Bin, Bin == 0, NA_real_),
      timer = y[Bin]
    ) %>%
    dplyr::filter((datetime - timer) < interval_length) %>%
    dplyr::select(-Bin)
}
