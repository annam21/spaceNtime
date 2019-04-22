
#' To do
#' Do I need an assume0 argument in STE? 
#' 
#' For timelapse photos: 
#' deploy should only have occasions, not intervals. 
#' Rewrite everything that depends on deploy 
#' I'll have to rewrite effort_fn to be able to take that. 
#' We can find effort on each occasion using the function I wrote in 
#' build_ise_eh. any NAs are when the camera wasn't functioning. 
#' Deploy can be broader. Say deploy = df. and it doesn't have interals, now it is each time. 
#' cam_occ_area <- effort_fn(deploy, occ)
#' left_join(area_occ_cam , df). and 
d <- as.POSIXct("2016-01-02 00:00:00", tz = "GMT")
df <- data.frame(
  cam = c(rep(1,3), rep(2,4)),
  datetime = d+c(3600*2, 3600*4, 3600*8, 3600*2, 3600*4, 3600*6, 3600*8),
  count = c(0,0,1,0,2,1,2),
  area = c(300,300,300,400,400,200,200)
)
deploy <- df
study_dates <- as.POSIXct(c("2016-01-02 00:00:00", "2016-01-02 23:59:59"), tz = "GMT")
occ <- build_occ(samp_freq = 3600*2, # seconds between each sampling occasion
                 samp_length = 10, # duration of each sampling occasion (seconds)
                 study_start = study_dates[1],
                 study_end = study_dates[2])
#'  
#' 
#' 
#' 
#' 
#' Future plans
#' For STE:
#' build in forced data subset (skip)
#' after subset, check that all cameras in df are in deploy (skip, because of timelapse photos)
#' Change exp_logl_fn to take a dataframe (skip, because not sure how to do for TTE)
#' Can you go through this whole workflow with a df subset by species? 
#' Make functions work if you have dates in deploy instead of POSIXct
#' create a data summary function for the user (maybe: summary(ste_eh) with class "STE")
#' Could force functions to pick one area in deploy if end of one line and start of next are the same
#' 
#' 
#' For IS:
#' 
#' For TTE: 
#' NA because camera wasn't working is different than NA because no animals were detected... 


