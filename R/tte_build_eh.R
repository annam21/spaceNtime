#' Build TTE encounter history
#'
#' @param df df object
#' @param deploy deploy object
#' @param occ tibble or dataframe specifying sampling occasions
#' @param samp_per 
#' @param ... optional arguments, including quiet = T to suppress time messages
#'
#' @return a dataframe with new columns for time-to-event and censor
#' @export
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
#' study_dates <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")
#' occ <- build_occ(samp_freq = 3600 * 10,
#'                  samp_length = 3600 * 10, 
#'                  study_start = study_dates[1],
#'                  study_end = study_dates[2]) 
#' tte_eh <- tte_build_eh(df, deploy, occ)
tte_build_eh <- function(df, deploy, occ, samp_per, ...){
  
  tictoc::tic("Data checks")
  # Data checks (exact same as STE)
  df <- validate_df(df)
  deploy <- validate_deploy(deploy)
  occ <- validate_occ(occ)
  
  # Forcing a data subset so I can validate df and deploy together. 
  # Subset is not technically necessary because everything hinges on occ later.
  d1 <- min(occ$start)
  d2 <- max(occ$end)
  df_s <- study_subset(df, "datetime", NULL, d1, d2)
  deploy_s <- study_subset(deploy, "start", "end", d1, d2)
  
  # Then validate df and deploy together (should really do after subset)
  validate_df_deploy(df_s, deploy_s) # This one is weird because it doesn't return anything if all good...
  
  tictoc::toc(...)
  
  # Build effort for each cam at each occasion
  tictoc::tic("effort")
  eff <- effort_fn(deploy_s, occ)
  tictoc::toc(...)
  
  # Calculate TTE and censor
  tictoc::tic("calculate TTE")
  out <- tte_calc_toevent(df, eff, samp_per)
  tictoc::toc(...)
  
  return(out)
}
