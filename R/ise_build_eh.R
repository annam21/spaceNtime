#' Build ISE encounter history
#'
#' @param df df object
#' @param deploy deploy object 
#' @param ... optional arguments, including quiet = T to suppress time messages
#' @param occ occ object
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
ise_build_eh <- function(df, deploy, occ, ...){

  tictoc::tic("data checks")
  # Run all my data checks here
  df <- validate_df(df)
  deploy <- validate_deploy(deploy)
  occ <- validate_occ(occ)
  validate_df_deploy(df, deploy) # This one doesn't return anything
  
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
  
  ### All lines until here are same as in STE... think about new fn. 
  
  tictoc::tic("build encounter history")
  # Build ISE EH
  ise <- eff %>% 
    left_join(., df_s, by = "cam") %>%
    filter(datetime %within% int) %>%
    select(occ, cam, count) %>%
    left_join(eff, ., by = c("occ", "cam")) %>%
    select(-int) %>% 
    mutate(count = replace(count, is.na(count) & area > 0, 0))
    # as long as area >0, deploy said the camera was on. So we're going to fill in count = 0
  tictoc::toc(...)
  
 return(ise)
}
