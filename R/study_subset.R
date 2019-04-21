#' Subset camera-related data
#' 
#' Keep rows of data where start_col and (optional) end_col lie within an 
#' inclusive interval defined by study_start and study_end
#'
#' @param x a dataframe or tibble with start and end columns
#' @param start_col the name of the start column in x 
#' @param end_col the name of the end column in x. Default = NULL
#' @param study_start the first part of the interval you want to subset by
#' @param study_end the end of the interval to subset by
#'
#' @return a filtered dataframe or tibble
#' @export
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
#' d <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-02 23:59:59"), tz = "GMT")
#' study_subset(df, "datetime", NULL, d[1], d[2])
#' 
study_subset <- function(x, start_col, end_col = NULL, study_start, study_end){
  # x is a data.frame with start_col and end_col; study_start and _end are study edges,
  # we want to only keep deployments that are within these
  if(is.null(end_col)){
    x %>%
      filter(between(!!as.name(start_col), study_start, study_end))
  } else {
    x %>%
      filter(!!as.name(start_col) <= study_end,
             !!as.name(end_col) >= study_start )
  }
}

### Checks: does this work for dates, times, and numbers? 
# What happens if start and end are the same (for either start and end)? (should return 0)
# Does it fail when the study end is before the study_start?  (Should return 0)
# Remember: this is a hard subset (must be fully WITHIN)


### Currently UNUSED