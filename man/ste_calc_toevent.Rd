% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ste_calc_toevent.R
\name{ste_calc_toevent}
\alias{ste_calc_toevent}
\title{Calculate STE on each occasion}
\usage{
ste_calc_toevent(df, occ, effort)
}
\arguments{
\item{df}{df object}

\item{occ}{occasions}

\item{effort}{effort from effort_fn()}
}
\value{
a dataframe with the same rows as occ, with a STE column
}
\description{
Calculate STE on each occasion
}
\examples{
  occ <- build_occ(samp_freq = 3600, 
            samp_length = 10,
            study_start = study_dates[1],
            study_end = study_dates[2])
 eff <- effort_fn(deploy, occ)
 ste_calc_toevent(df, occ, eff)
}
