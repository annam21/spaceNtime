  # Rewrite package entirely, for staggered entry

#' Future plans:
#' build in forced data subset (skip)
#' after subset, check that all cameras in df are in deploy (skip, because of timelapse photos)
#' Change exp_logl_fn to take a dataframe (skip, because not sure how to do for TTE)
#' Can you go through this whole workflow with a df subset by species? 
#' Make functions work if you have dates in deploy instead of POSIXct
#' create a data summary function for the user (maybe: summary(ste_eh) with class "STE")
#' push to github
#' change package name
#' Then move on to ISE! 


# data summary for user



################################################################################ 
  # Workflow

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

  # Do all the functions work if deploy has dates instead of POSIX?
  # deploy <- data.frame(cam = c(1, 2, 2),
  #                      start = as.Date(c("2015-12-01", "2016-01-01", "2016-01-02")),
  #                      end = as.Date(c("2016-01-05", "2016-01-01", "2016-01-05")),
  #                      area = c(300,300,450))

  
  
################################################################################
  # # Okay... I'm really proud of that but now realizing I don't need it... 
  # repl <- function(x, start_col, end_col, study_start, study_end){
  #   # x is a data.frame with start_col and end_col; study_start and _end are hard edges, 
  #   # we want to replace earlier times and later times with these. 
  #   x %>%
  #     filter(!!as.name(start_col) <= study_end, 
  #            !!as.name(end_col) >= study_start ) %>% 
  #     mutate(!!start_col := pmax(!!as.name(start_col), study_start),
  #            !!end_col := pmin(!!as.name(end_col), study_end) )
  # }

  
  # Function for rounding down to when intervals start
  # Probably not that useful. It would be nicer to use lubridate::floor_date("15 seconds")
  # rnd_dn <- function(x, samp_length){
  #   # x is a vector, samp_length is samp_length
  #   timz <- lubridate::tz(x)
  #   out <- as.POSIXct(floor(as.numeric(df$datetime)/samp_length)*samp_length, origin = "1970-01-01", tz = timz)
  #   return(out)
  # }
  # rnd_dn(df$datetime, samp_length = 100)
  
  # subs <- function(x, study_dates){
  #   # int is name of interval column 
  #   dates <- interval(study_dates[1], study_dates[2])
  #   out <- x %>% 
  #     filter(int %within% dates)
  #   return(out)
  # }
  # purrr::map(dat_init[2:3], subs(study_dates = study_dates))
  
  # Join two tables by intervals. B = df with big interval. L = df with little interval. 
  # This keeps all rows of the little interval. 
  # B-int and L_int and joincol are colnames
  # ntrvl_join <- function(B, L, B_int, L_int, joincol){
  #   L %>% 
  #     left_join(., B, by = joincol) %>% 
  #     filter(!!as.name(L_int) %within% !!as.name(B_int)) %>%
  #     select(!!as.name(joincol), occ) %>%  # What if I only keep cam? Nope,need occ. 
  #     mutate(effort = 1) %>% 
  #     left_join(L, ., by = c("cam", "occ")) %>% 
  #     mutate(effort = replace(effort, is.na(effort), 0))
  # }
  # What if intervals are same name? What if different name? 
  # ntrvl_join(occ_cam, deploy, "occ_int", "depl_int", "cam")
  
  # effort <- occ_cam %>%
  #   left_join(., dat_subset$deploy, by = "cam") %>%
  #   filter(occ_int %within% int) %>%
  #   select(occ, cam) %>%
  #   mutate(effort = 1) %>%
  #   left_join(occ_cam, ., by = c("occ", "cam")) %>%
  #   mutate(effort = replace(effort, is.na(effort), 0))
  # # Make this more general for any two dataframes with intervals...
  # 
  # # 3.2) Define area at each occasion
  # # deploy has the big interval. occ_cam has the small intervals. 
  # # occ_cam$occ_int %within% deploy$int 
  # # For each interval in deploy, join the two tables. 
  # occ_area <- occ_cam %>%
  #   left_join(., dat_subset$deploy, by = "cam") %>%
  #   filter(occ_int %within% int) %>%
  #   select(occ, cam, area) %>% 
  #   left_join(occ_cam, ., by = c("occ", "cam")) %>% 
  #   mutate(area = replace(area, is.na(area), 0) )
  # 

  # # 3.1) Define effort (from deploy) and area (from deploy) at each occasion 
  # # combine effort and camera area by occasion
  # effort <- occ_cam %>%
  #   left_join(., dat_subset$deploy, by = "cam") %>%
  #   filter(occ_int %within% int) %>%
  #   select(occ, cam, working) 
  # occ_area <- occ_cam %>%
  #   left_join(., dat_subset$deploy, by = "cam") %>%
  #   filter(occ_int %within% int) %>%
  #   select(occ, cam, area)
  # combo <- occ_cam %>% 
  #   left_join(., effort) %>% 
  #   left_join(., occ_area) %>%
  #   mutate_at(vars(working, area), ~tidyr::replace_na(., 0)) %>% 
  #   mutate(area = working * area)
  
  # deploy <- data.frame(
  #   cam = c(1, 2, 2),
  #   start = as.POSIXct(c("2015-12-01 00:00:00", "2016-01-01 00:00:00", "2016-01-02 00:00:00"), tz = "GMT"),
  #   end = as.POSIXct(c("2016-01-05 00:00:00", "2016-01-01 12:00:00", "2016-01-05 00:00:00"), tz = "GMT"),
  #   working = 1
  # )
  
  
  # dd <- list(df = df, 
  #            deploy = deploy, 
  #            occ = occ) %>% 
  #   purrr::map_if(., ~chck_names(., c("start", "end")), add_int)