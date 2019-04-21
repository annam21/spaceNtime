#' Add interval column
#'
#' @param x a dataframe or tibble with columns "start" and "end"
#'
#' @return a dataframe or tibble
#' @export
#'
#' @examples add_int(deploy)
add_int <- function(x){
  x %>% 
    mutate(int = lubridate::interval(start, end) )
}
