# Anna Moeller
# Starting TTE code
# 11/1/2019

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

# note to self: I don't think TTE area can change within an occasion 

occ <- build_occ(samp_freq = 3600 * 10, # seconds between the start of each sampling occasion
                 samp_length = 3600 * 10, # duration of each sampling occasion (seconds)
                 study_start = study_dates[1],
                 study_end = study_dates[2]) 

# Sampling period length
per <- tte_samp_per(deploy, lps = 0.0083)

# Building encounter history
# in old code, TTE encounter history was a matrix with ncam rows and nocc cols

tte_eh <- tte_build_eh(df, deploy, occ, per)

# Estimate abundance
tte_estN_fn(tte_eh, study_area = 1e6)
