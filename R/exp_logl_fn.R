#' Exponential log likelihood
#' 
#' The log likelihood of the exponential distribution that is used for STE and TTE. 
#' 
#' For TTE, x$toevent will be a matrix with 1 row per camera and 1 column per sampling occasion. 
#' For STE, x$toevent will be a matrix with 1 row. 1 column per sampling occasion  
#'
#' @param x A list formulated by ste_data_fn or tte_data_fn. 
#' x$toevent is a matrix with space- or time-to-event and NAs.
#' x$censor is the censor
#' @param param The value of log(lambda) of which you want to evaluate the logL. 
#'
#' @return The MLE value of lambda-hat
#' @export
#'
#' @examples
#' x <- build_ste_eh(df, deploy, occ)
#' dat.ste <- list(toevent = matrix(x$STE, nrow = 1),
#'                 censor = x$censor)
#' optim(log(1/mean(dat.ste$toevent, na.rm = T)), 
#'              exp_logl_fn, 
#'              x = dat.ste, 
#'              control = list(fnscale = -1),
#'              hessian = T)
#'   
exp_logl_fn <- function(x, param){
  # param: beta parameter for lambda
  lambda <- exp(param) 
  logL <- 0
  for(i in 1:nrow(x$toevent)) {
    for (j in 1:ncol(x$toevent)) {
      if(!is.na(x$toevent[i, j])) {
        tmp <- dexp(x$toevent[i, j], lambda)
      } else {
        tmp <- pexp(x$censor[j], lambda, lower.tail = F)
      }
      logL <- logL + log(tmp)
    }
  }
  return(logL)
}
