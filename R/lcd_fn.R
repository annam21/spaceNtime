#' Find the lowest common denominator between occasions 
#'
#' @param occ occasions, a dataframe
#'
#' @return Returns a value: either the lowest common denominator between occasions 
#' if they are evenly spaced (equal to samp_freq if occ is built by build_occ) 
#' or 0 if occasions are not evenly spaced
#' @export
#'
#' @examples
#'   occ <- build_occ(samp_freq = 3600, 
#'             samp_length = 10,
#'             study_start = study_dates[1],
#'             study_end = study_dates[2])
#'  findfrq <- lcd_fn(occ)
lcd_fn <- function(occ){
  t <- occ %>% 
    mutate(d = as.numeric(difftime(start, lag(start), units = "secs")),
           m = min(d, na.rm = T),
           r = d %% m)
  
  if(any(t$r != 0, na.rm = T)){
    return(0)
  } else {
    return(t$m[1])
  }
}

# I know, I know. This takes a dataframe and returns a value. Can clean in future.