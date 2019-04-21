
#' To do
#' finish and document build_ise_eh.R
#' Do I need an assume0 argument in STE? 
#' Rename functions to match. I think ise_estN_fn, ise_build_eh, etc. 
#' Add variance estimator for ISE 
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
#' 
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
#


