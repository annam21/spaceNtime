
#' To do
#' 
#' Look at tte_build_eh and make sure it works 
#' Change all intervals to rounding if possible. 
#' hide internal functions 
#' GitHub pages for the vignette. 
#' estNfn output density, make abundance a separate function 
#' ste_calc_toevent doesn't need occ in function call 
#' tte_estN_fn is basically the same as ste_estn_fn
#' Bootstrap function for STE (using random sampling for occasions?)
#' Speed up tte_build_eh to match STE
#' 
#' 
#' 
#' 
#' What does ste_estN_fn do if censor = 0? It doesn't fail... Is this good? 
#' 
#' Test that version 2 of deploy actually works. 
#' 
#' For timelapse photos: 
#' We can find effort on each occasion using the function I wrote in 
#' build_ise_eh. any NAs are when the camera wasn't functioning. 
#' Deploy can be broader. Say deploy = df. and it doesn't have interals, now it is each time. 
#' cam_occ_area <- effort_fn(deploy, occ)
#' left_join(area_occ_cam , df). and 
#' 
#' validate_df_deploy very similar function to find_overlap. Work on that in future
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


