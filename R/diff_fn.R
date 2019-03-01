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
#' df <- data.frame(ID = 1:4, Value = c(1, 5, 10, 8))
#' y <- c(2,4,6,8)
#' dplyr::mutate(df, newcol = diff_fn(Value, y, 1))
#'
diff_fn <- function(x, y, interval_length){
  # For every x, pull out the first y where
  purrr::map_dbl(x,
          ~ subset(y, (only_pos(as.numeric(.x) -
                                  as.numeric(y)) <= interval_length))[1]
  )
}
