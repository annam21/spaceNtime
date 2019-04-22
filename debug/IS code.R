# IS
library(dplyr)
devtools::load_all(".")

# What we actually have...
# Date the user needs to feed in 
df <- data.frame(
  cam = c(1,1,2,2,2),
  datetime = as.POSIXct(c("2016-01-02 12:00:00",
                          "2016-01-03 13:12:00",
                          "2016-01-02 12:00:00",
                          "2016-01-02 14:00:00",
                          "2016-01-03 16:53:42"),
                        tz = "GMT"),
  count = c(1, 0, 2, 1, 2)
)
deploy <- data.frame(
  cam = c(1, 2, 2, 2),
  start = as.POSIXct(c("2015-12-01 15:00:00",
                       "2015-12-08 00:00:00", 
                       "2016-01-01 00:00:00", 
                       "2016-01-02 00:00:00"),
                     tz = "GMT"),
  end = as.POSIXct(c("2016-01-05 00:00:00", 
                     "2015-12-19 03:30:00", 
                     "2016-01-01 05:00:00",
                     "2016-01-05 00:00:00"), 
                   tz = "GMT"),
  area = c(300, 200, 200, 450)
)

# Build an effort function for timelapse photos

# Write an effort function for timelapse, using df
effort_T_fn <- function(df, occ, working_col = NULL){
  
  # Create occasions by ALL cameras (do before subset step)
  occ_by_cam <- build_occ_cam(df, occ) %>%
    add_int(.) 
  
  # Subset df down to functioning photos (optional)
  if(!is.null(working_col)){
    df <- df %>%
      filter(!!as.name(working_col) == T)
  }
  
  # Put the two together  
  eff <- left_join(occ_by_cam, df, by = "cam") %>%
    filter(datetime %within% int) %>%
    select(occ, cam, count) %>%
    left_join(occ_by_cam, ., by = c("occ", "cam"))
}





# IS bootstrap
is_boot_fn <- function(cam_occ_EH, steps_btw_samples, A, nboot){
  
  bootN <- rep(NA, nboot)
  for(i in 1:nboot){
    # Sample cams with replacement (thus spread then gather): make a new eh
    lu <- cam_occ_EH %>% 
      select(cam, step, a)
    bootdf <- cam_occ_EH %>% 
      select(-a) %>% 
      spread(step, nanimals) %>%
      sample_n(size = nrow(.), replace = T) %>%
      gather(step, nanimals, -cam) %>%
      mutate(step = as.numeric(step)) %>% 
      left_join(., lu, by = c("cam", "step"))
    
    
    # Estimate abundance on it
    bootN[i] <- is_estN_fn(bootdf, steps_btw_samples, A)$N
  }
  
  SE_is_estN <- sd(bootN)
  CI_is_estN <- quantile(bootN, c(0.025, 0.975))
  
  return(list(SE_is_estN = SE_is_estN, 
              CI_is_estN = CI_is_estN) )
}




