#' Calculate time-to-event
#'
#' @param df df object
#' @param effort effort from effort_fn()
#' @param samp_per sampling period length. The mean length of time for an animal 
#' to pass through a camera viewshed
#'
#' @return a dataframe with the same rows as occ, with a TTE column
#' @export
#'
#' @examples
#' df <- data.frame(
#'   cam = c(1,1,2,2,2),
#'   datetime = as.POSIXct(c("2016-01-02 12:00:00",
#'                         "2016-01-03 13:12:00",
#'                         "2016-01-02 12:00:00",
#'                         "2016-01-02 14:00:00",
#'                         "2016-01-03 16:53:42"),
#'                       tz = "GMT"),
#'   count = c(1, 0, 2, 1, 2)
#' )
#' eff <- effort_fn(deploy, occ)
#' tte_calc_toevent(df, eff)
tte_calc_toevent <- function(df, effort, samp_per){
  out <- df %>%
    filter(count > 0) %>%
    left_join(effort, .,  by = "cam") %>% 
    filter(datetime %within% int) %>%
    
    # Take only the first event in the sampling occasion
    group_by(cam, occ) %>% 
    filter(!duplicated(occ)) %>% 
    
    # Join back up with all cams and occasions 
    select(cam, occ, datetime, count) %>% 
    left_join(effort, ., by = c("occ", "cam")) %>% 
    # select(cam, occ, start, end, int, area) %>% 
    arrange(cam, occ) %>%
    
    # Calculate TTE
    mutate(TTE = as.numeric(datetime) - as.numeric(start),
           TTE = TTE/samp_per*area) %>%
    
    # Calculate censor
    mutate(censor = as.numeric(end) - as.numeric(start),
           censor = censor/samp_per*area) 
  return(out)
}