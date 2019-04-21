#' Build STE encounter history
#'
#' @param df df object 
#' @param deploy deploy object
#' @param occ tibble or dataframe specifying sampling occasions
#' @param study_area the size of the study area (same units as camera viewshed)
#'
#' @return a list, for feeding into ste_estn_fn
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
#' build_ste_eh(df, deploy, occ, study_area = 1e6)
#' 
build_ste_eh <- function(df, deploy, occ, study_area){
  # $df and $effort and $occ from dat
  # A is your study area size.
 
   # Run all my data checks here
  df <- validate_df(df)
  deploy <- validate_deploy(deploy)
  occ <- validate_occ(occ)
  validate_df_deploy(df, deploy) # This one is weird because it doesn't return anything...
  
  # Build effort for each cam at each occasion
  eff <- effort_fn(deploy, occ)
  
  # Calculate the censors
  censor <- calc_censor(eff)

  # Calculate STE at each occasion
  tmp <- calc_ste(df, occ, eff)   

  out <- list(toevent = matrix(tmp$STE, nrow = 1),
              censor = censor$censor,
              A = study_area )
  
  return(out)
}