#' Build ISE encounter history
#'
#' @param df df object
#' @param deploy deploy object 
#' @param occ occ object
#' @param assume0 if T: assume that no picture = no animal. If F: assume that no picture = camera not working
#'
#' @return a data frame with encounter history for instantaneous sampling 
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
#' deploy <- data.frame(
#'   cam = c(1, 2, 2, 2),
#'   start = as.POSIXct(c("2015-12-01 15:00:00",
#'                        "2015-12-08 00:00:00", 
#'                        "2016-01-01 00:00:00", 
#'                        "2016-01-02 00:00:00"),
#'                      tz = "GMT"),
#'   end = as.POSIXct(c("2016-01-05 00:00:00", 
#'                      "2015-12-19 03:30:00", 
#'                      "2016-01-01 05:00:00",
#'                      "2016-01-05 00:00:00"), 
#'                    tz = "GMT"),
#'   area = c(300, 200, 200, 450)
#' )
#'occ <- build_occ(samp_freq = 3600, 
#'             samp_length = 10,
#'             study_start = study_dates[1],
#'             study_end = study_dates[2])
#' ise_build_eh(df, deploy, occ, assume0 = T)
ise_build_eh <- function(df, deploy, occ, assume0 = T){
  # This is currently for deploy version 2 only 
  # Assume0 = T if you want to assume no picture = 0 animals
  # Assume0 = F if you want to assume no picture = not working. 
  
  # Run all my data checks here
  df <- validate_df(df)
  deploy <- validate_deploy(deploy)
  occ <- validate_occ(occ)
  validate_df_deploy(df, deploy) # This one doesn't return anything
  
  # I could force a data subset here, but it all hinges on occ. 
  # If occ is correct, everything else will be. 
  
  # Build effort for each cam at each occasion
  eff <- effort_fn(deploy, occ)
  
  ### All lines until here are same as in STE... think about new fn. 
  
  # Build ISE EH
  ise <- eff %>% 
    left_join(., df, by = "cam") %>%
    filter(datetime %within% int) %>%
    select(occ, cam, count) %>%
    left_join(eff, ., by = c("occ", "cam")) %>%
    select(-int)
  
  # timelapse: NA means that camera wasn't on. 
  # motion sensor: we will assume NA should be 0
  if(assume0){
    ise <- ise %>% 
      mutate(count = replace(count, is.na(count), 0)) 
  } else {
    ise <- ise %>% 
      mutate(area = replace(area, is.na(count), 0))
  }
 return(ise)
}
