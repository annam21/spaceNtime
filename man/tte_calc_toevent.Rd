% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tte_calc_toevent.R
\name{tte_calc_toevent}
\alias{tte_calc_toevent}
\title{Calculate time-to-event}
\usage{
tte_calc_toevent(df, effort, samp_per)
}
\arguments{
\item{df}{df object}

\item{effort}{effort from effort_fn()}

\item{samp_per}{sampling period length. The mean length of time for an animal 
to pass through a camera viewshed}
}
\value{
a dataframe with the same rows as occ, with a TTE column
}
\description{
Calculate time-to-event
}
\examples{
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
eff <- effort_fn(deploy, occ)
tte_calc_toevent(df, eff)
}
