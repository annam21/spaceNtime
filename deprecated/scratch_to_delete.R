# scratch work
# Kenneth Loonam
# July 2020

#STE_EXP_Workflow===============================================================
# require(spaceNtime); require(dplyr); require(purrr)

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
n_bins <- c(2,3,5)
lambda <- 0.001
bin_cuts <- "fixed"

exp_gof_STE(eh, n_bins, lambda)

#STE_pois_workflow==============================================================

pois_gof_STE(n_bins, lambda, df, deploy, occ)


#TTE_Workflow===================================================================

study_dates <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")
occ <- build_occ(samp_freq = 3600 * 10,
                 samp_length = 3600 * 8,
                 study_start = study_dates[1],
                 study_end = study_dates[2])

per <- tte_samp_per(deploy, lps = 30/3600)

tte_eh <- tte_build_eh(df, deploy, occ,  per)
eh <- tte_eh

exp_gof_TTE(eh, n_bins, lambda)
pois_gof_TTE(n_bins, lambda, eh)
