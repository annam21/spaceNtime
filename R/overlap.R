# Functions to find overlapping intervals in a dataframe

#' Find if a single interval overlaps multiple intervals
#'
#' @param one_int a single lubridate::interval
#' @param n_int a vector of lubridate::intervals
#'
#' @return a single value, the number of intervals that one_int overlaps
#' @export
#'
#' @examples 
#' s <- Sys.time()
#' a <- lubridate::interval((s-5), (s))
#' b <- lubridate::int_diff(c((s-4),(s+1),(s+5)))
#' overl(a, b)
overl <- function(one_int, n_int){
  # Finds out if any single interval overlaps multiple intervals
  sum(lubridate::int_overlaps(one_int, n_int)) != 1
  # non-overlapping intervals will have sum = 1
}

#' Find cameras with overlapping intervals
#'
#' @param x a dataframe or tibble, with columns cam, start and end
#'
#' @return a dataframe with any records that are overlapping (grouped by cam)
#' @export
#' @import dplyr
#'
#' @examples 
#' find_overlap(deploy)  # validation step 
find_overlap <- function(x){
  out <- x %>%
    assertr::verify(has_all_names("cam", "start", "end")) %>%
    mutate(int = lubridate::interval(start, end)) %>%
    group_by(cam) %>% 
    filter(n() > 1) 
  # If there is more than one row per camera, check overlap
  if(nrow(out) != 0){
    out <- out %>%
      mutate(chk = list(int)) %>% 
      rowwise() %>% # If this returns empty dataframe, the next step fails. 
      mutate(overlap = overl(int, chk)) %>%
      filter(overlap) %>%
      select(-int, -chk, -overlap)
  }
  return(out)
}
