# Script for figuring out data checks.
 
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





# More questions... 
# Deploy: 
# What happens if these are Dates instead of times? 
# What happens if a start and end in one line are the same?


# 
# # Data summary 
# #   Summarise the number of cameras in each... later after subset
# length(unique(df$cam))
# length(unique(df$deploy))