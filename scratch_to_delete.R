# scratch work
# Kenneth Loonam
# July 2020

#Recreate_vignette_objects======================================================
require(spaceNtime); require(dplyr); require(purrr); require(assertr)

df <- data.frame(
  cam = c(1,1,2,2,2),
  datetime = as.POSIXct(c("2016-01-02 12:00:00",
                          "2016-01-03 13:12:00",
                          "2016-01-02 12:00:00",
                          "2016-01-02 14:00:00",
                          "2016-01-03 16:53:42"),
                        tz = "GMT"),
  count = c(1, 0, 0, 1, 2)
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

study_dates <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")

occ <- build_occ(samp_freq = 3600, # seconds between the start of each sampling occasion
                 samp_length = 10, # duration of each sampling occasion (seconds)
                 study_start = study_dates[1],
                 study_end = study_dates[2])

eh <- ste_build_eh(df, deploy, occ)
n_bins <- 5
lambda <- 0.001
bin_cuts <- "equal"
source("R\\build_gof_bins_eq.R"); source("R\\build_gof_bins_var.R")
source("R\\gof_bins_var_int.R"); source("R\\exponential_density.R")
source("R\\assign_bins.R")
