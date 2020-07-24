  # Speed up model using real data
  # Anna Moeller 
  # 7/1/2020
  
  library(tidyverse)
  # Load pictures
  load("../CameraTrapStudy/2015 data/pics.wide20160804.RData")

  # Make dataframe 
  df <- pics %>%
    select(cam, timeGMT, elkpresent) %>% 
    filter(elkpresent == T) %>% 
    mutate(count = as.numeric(elkpresent)
    ) %>% 
    rename(datetime = timeGMT)
  
  # Make a pretend deploy, as if all always working 
  deploy <- pics %>% 
    distinct(cam, op.start, op.end) %>% 
    # fix problem child
    mutate(op.start = replace(op.start, cam == "AM158", "2016-01-05 17:00:00") ) %>%
    mutate(start = as.POSIXct(op.start),
           start = lubridate::force_tz(start, "GMT"),
           end = as.POSIXct(op.end),
           end = lubridate::force_tz(end, "GMT"),
           area = 250
    ) %>%
    select(-op.start, -op.end) 
  
  study_dates <- as.POSIXct(c("2016-01-01 01:04:18", "2016-03-27 00:00:00"), 
                            tz = "GMT")
  
  # Now actually use the package 
  occ <- build_occ(samp_freq = 40000,
                   samp_length = 1,
                   study_start = study_dates[1],
                   study_end = study_dates[2])
  
  
# Problem 1. speed up effort_fn. 
  # Problem is in group_by and summarize to get rid of multiple rows 
  # I want to fix the problem by getting rid of the second row. 
  
  # # play with deploy to debug 
  # deploy <- tibble::tibble(
  #   cam = c("AM174", "AM174"),
  #   start = as.POSIXct(c("2016-01-06 17:00:00", "2016-01-08 17:00:02"), tz = "GMT"),
  #   # end = "2016-01-08 17:00:01", "2016-01-10 18:00:00" ), tz = "GMT"),
  #   area = c(250, 100)
  # )

  # Test it out  
  ste_eh <- ste_build_eh(df, deploy, occ, quiet = T)
  
##################################################################################
  # Time to event 
  tte_occ <- build_occ(samp_freq = 3600*10,
                      samp_length = 3600*8, 
                      study_start = study_dates[1],
                      study_end = study_dates[2])
  
  #()
  effort <- effort_fn(deploy, tte_occ)
  
  # Run it
  per <- tte_samp_per(deploy, lps = 36/3600)
  tte_eh <- tte_build_eh(df, deploy, tte_occ, per)  
  
  
  