# Script for figuring out data checks.

# Normal, motion-sensor
df <- data.frame(
  cam = c(1, 1, 2, 2, 2, 2, 2, 3),
  datetime = as.POSIXct(c("2016-01-02 12:00:00",
                          "2016-01-03 13:12:00",
                          "2016-01-02 12:00:00",
                          "2016-01-02 12:00:06",
                          "2016-01-02 14:00:00",
                          "2016-01-02 14:00:05",
                          "2016-01-03 16:53:42",
                          "2016-01-02 14:00:00"),
                        tz = "GMT"),
  count = c(1, 0, 0, 1, 1, 1, 2, 1)
)
deploy <- data.frame(
  cam = c(1, 2, 2, 2, 3),
  start = as.POSIXct(c("2015-12-01 15:00:00",
                       "2015-12-08 00:00:00", 
                       "2016-01-01 00:00:00", 
                       "2016-01-02 00:00:00",
                       "2016-01-01 00:00:00"),
                     tz = "GMT"),
  end = as.POSIXct(c("2016-01-05 00:00:00", 
                     "2015-12-19 03:30:00", 
                     "2016-01-01 05:00:00",
                     "2016-01-05 00:00:00",
                     "2016-01-05 00:00:00"), 
                   tz = "GMT"),
  area = c(300, 200, 200, 450, 300)
)

 
# Overlapping time intervals 
deploy <- data.frame(
  cam = c(1, 2, 2, 2),
  start = as.POSIXct(c("2015-12-01 15:00:00",
                       "2015-12-08 00:00:00",
                       "2015-12-18 00:00:00",
                       "2016-01-02 00:00:00"),
                     tz = "GMT"),
  end = as.POSIXct(c("2016-01-05 00:00:00",
                     "2015-12-19 03:30:00",
                     "2016-01-01 05:00:00",
                     "2016-01-05 00:00:00"),
                   tz = "GMT"),
  area = c(300, 200, 150, 450)
)

# df and deploy for timelapse 
df_T <- data.frame(
  cam = c(1,1,2,2,2),
  datetime = as.POSIXct(c("2016-01-02 12:00:00",
                          "2016-01-02 14:00:00",
                          "2016-01-02 12:00:00",
                          "2016-01-02 14:00:00",
                          "2016-01-03 16:00:00"),
                        tz = "GMT"),
  count = c(0, 1, 0, 0, 2),
  area = c(300, 300, 200, 0, 450)
)
deploy_T <- df_T %>%
  mutate(start = datetime, 
         end = start) %>%
  select(cam, start, end, area)

# df and deploy for Motion + Timelapse
df <- data.frame(
  cam = c(1,1,2,2,2,2),
  datetime = as.POSIXct(c("2016-01-02 12:00:00",
                          "2016-01-02 14:00:00",
                          "2016-01-02 12:00:00",
                          "2016-01-02 12:00:06",
                          "2016-01-02 14:00:00",
                          "2016-01-03 16:00:00"),
                        tz = "GMT"),
  count = c(0, 1, 0, 3, 0, 2),
  area = c(300, 300, 200, 200, 0, 450)
)

deploy <- df %>%
  mutate(start = datetime, 
         end = start) %>%
  select(cam, start, end, area)


# Try a deploy that's missing one of my cameras that DID take a photo
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
  end = as.POSIXct(c("2016-01-01 00:00:01",
                     "2015-12-19 03:30:00", 
                     "2016-01-01 05:00:00",
                     "2016-01-05 00:00:00"), 
                   tz = "GMT"),
  area = c(300, 200, 200, 450)
)




# Do all the functions work if deploy has dates instead of POSIX?
deploy <- data.frame(cam = c(1, 2, 2),
                     start = as.Date(c("2015-12-01", "2016-01-01", "2016-01-02")),
                     end = as.Date(c("2016-01-05", "2016-01-01", "2016-01-05")),
                     area = c(300,300,450))


# # Data summary 
# #   Summarise the number of cameras in each... later after subset
# length(unique(df$cam))
# length(unique(df$deploy))