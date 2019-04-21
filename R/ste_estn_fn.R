#' Estimate abundance from STE
#' 
#' A function to estimate abundance from an STE encounter history
#'
#' @param x A list formulated by ste_data_fn or tte_data_fn. 
#' x$toevent is a matrix with space- or time-to-event and NAs.
#' x$censor is the censor
#' x$A is the size of the study area, in the same units as a 
#' @return A list, with the estimated abundance with its standard error and confidence intervals.
#' @export
#'
#' @examples
#' df <- data.frame(cam = c(1,1,2,2),
#'             datetime = as.POSIXct(c("2016-01-02 12:00:00",
#'                                     "2016-01-03 13:12:00",
#'                                     "2016-01-02 14:00:00",
#'                                     "2016-01-03 16:53:42"),
#'                                    tz = "GMT"),
#'                   a = c(850, 850, 1100, 1100),
#'                   count = c(1, 0, 0, 2))
#' tab <- a_lookup_fn(df)
#' d <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")
#' dat.ste <- ste_data_fn(df,
#'             count_col = "count",
#'             samp = 3600,
#'             samp_length = 10,
#'             cam_areas = tab,
#'             date_lim = d,
#'             A = 150000)
#' ste_estN_fn(dat.ste)
ste_estN_fn <- function(x){
  opt <- suppressWarnings(
    stats::optim(log(1/mean(x$toevent, na.rm = T)), 
               exp_logl_fn, 
               x = x, 
               control = list(fnscale = -1),
               hessian = T)
  )
  
  # Estimate of lambda
  estlam <- exp(opt$par)
  
  # estlam is average density per m2
  estN <- estlam * x$A
  
  # Delta method for variance
  varB <- -1 * MASS::ginv(opt$hessian)
  form <- sprintf("~ %f * exp(x1)", x$A)
  SE_N <- msm::deltamethod(g = stats::as.formula(form), mean = opt$par, cov = varB, ses = T)
  
  logCI <- exp( 1.96*sqrt(log(1 + (SE_N/estN)^2 )))
  
  return(list(estN = estN,
              SE_N = SE_N,
              LCI = estN / logCI,
              UCI  = estN * logCI ) )
}