  # Rewrite package entirely, for staggered entry

  
  devtools::load_all(".") # Load current package. 
  library(dplyr)
  library(lubridate)



  # 2) Build occasions
  build_occ <- function(samp_freq, samp_length, study_start, study_end){
    # Occasions without camera
    data.frame(
      start = sampling_start(samp_freq = samp_freq,
                             date_lim = c(study_start, study_end))
      ) %>%  
      mutate(end = start + samp_length,
             occ = 1:n()) %>% 
      select(occ, start, end) 
  }

  # Occasions by camera
  build_occ_cam <- function(df, occ, new_elem){
    # dat = a list, including $df and $occ
    tidyr::crossing(
      occ = occ$occ, 
      cam = deploy$cam
      ) %>% 
      left_join(., occ, by = "occ") 
  }
  
  # occ_cam <- build_occ_cam(dat_subset) # Example call
  
  # 2.1) Create lubridate intervals in the data (for later use)
  chck_names <- function(x){
    "start" %in% names(x) & "end" %in% names(x)
  }
  add_int <- function(x){
    x %>% 
      mutate(int = start %--% end)
  }
  # dat_subset %>% 
  #   purrr::map_if(., chck_names, add_int) # Example call
    
  
  # 3) Find the area of working cameras at each occasion
  # 3.1) Define effort (from deploy) and area (from deploy) at each occasion
  # combine effort and camera area by occasion
  effort_fn <- function(df, deploy, occ){
    
    # Create intervals in data
    dd <- list(df = df, 
               deploy = deploy, 
               occ = occ) %>% 
      purrr::map_if(., chck_names, add_int)
    
    # Create occasions by camera
    dd$effort <- build_occ_cam(dd$df, dd$occ, "effort") 
    
    # Combine these two
    dd$effort <- dd$effort %>%
      rename(occ_int = int) %>% 
      left_join(., dd$deploy, by = "cam") %>%
      filter(occ_int %within% int) %>%
      select(occ, cam, area) %>% 
      left_join(dd$effort, ., by = c("occ", "cam")) %>% 
      mutate(area = replace(area, is.na(area), 0))
    return(dd$effort)
  }

  # 3.2) Calculate censor for each time step
  calc_censor <- function(effort){
    # Just requires effort df 
    effort %>% 
      group_by(occ) %>% 
      summarise(censor = sum(area))
  }
  
  # 4) Compute STE on each occasion 
  ste_data_fn <- function(df, occ, effort, A){
    # $df and $effort and $occ from dat
    # A is your study area size.
    
    # First, calculate your censors
    censor <- calc_censor(effort)
    
    tmp <- df %>%
      # Find count at each sampling occasion
      left_join(effort, .,  by = "cam") %>% 
      filter(datetime %within% int) %>% 
      
      # Randomly order the cameras at each occasion
      filter(area != 0) %>% 
      group_by(occ) %>% 
      sample_n(n()) %>%
      
      # Find the area until the first count, at each occcasion
      mutate(STE = cumsum(area)) %>% 
      filter(count > 0) %>% 
      filter(!duplicated(occ)) %>% 
      
      # Add back NAs on all other sampling occasions
      select(occ, STE) %>%
      left_join(occ, .) %>%  
      
      # Add in our censors
      left_join(., censor)    
   
    if(all(is.na(tmp$STE))) warning("No animals detected in any sampling occasion")
    
    out <- list(toevent = matrix(tmp$STE, nrow = 1),
                censor = tmp$censor,
                A = A )
    return(out)
  }


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

  # Do all the functions work if deploy (deploy) has dates instead of POSIX?
  # deploy <- data.frame(cam = c(1, 2, 2),
  #                      start = as.Date(c("2015-12-01", "2016-01-01", "2016-01-02")),
  #                      end = as.Date(c("2016-01-05", "2016-01-01", "2016-01-05")),
  #                      area = c(300,300,450))
  
  # Workflow
  
  # 1) Build occasions
  study_dates <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")
  occ <- build_occ(samp_freq = 3600, 
                              samp_length = 10,
                              study_start = study_dates[1],
                              study_end = study_dates[2])
  
  # 2) Data checks and 3) define effort
  effort <- effort_fn(df, deploy, occ)
  
  # 4)  think the subset check might go here. Make sure all pieces of dat are subset
  #  Put together data in a list 
  dat <- list(df = df, deploy = deploy)
  dat_subset <- mapply(study_subset,
                       dat_init,
                       start_col = c("datetime", "start"),
                       end_col = list(NULL, "end"), # list is key for passing a NULL. can't use c()
                       study_start = study_dates[1],
                       study_end = study_dates[2])
  # study_dates <- as.POSIXct(c("2015-01-05 00:00:00", "2015-01-07 23:59:59"), tz = "GMT")
  # study_dates <- as.POSIXct(c("2016-01-05 00:00:00", "2016-01-07 23:59:59"), tz = "GMT")
  
  # Tests
  # study_subset(deploy, "start", "end", study_dates[1], study_dates[2])
  # study_subset(df, "datetime", end_col = NULL, study_dates[1], study_dates[2])
  
  # Make your data
  dat_ste <- ste_data_fn(dat_subset$df, dat_subset$occ, dat_subset$effort, 1e6)
  
  # Give the user summaries about their data 
  
  # Estimate abundance
  ste_estN_fn(dat_ste)
  

  # Can you go through this whole workflow with a df subset by species? 
  # In the future, I can make exp_logl_fn work on a df. 
  
  
  
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