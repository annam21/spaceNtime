#' Log confidence intervals
#' 
#' Returns confidence intervals for abundance that do not go below 0
#'
#' @param estimate parameter estimate
#' @param se estimate parameter standard error
#'
#' @return a data frame with LCI and UCI
#' @export
#'
#' @examples logCI(32, 25)
logCI <- function(estimate, se){
  ci <- exp( 1.96*sqrt(log(1 + (se/estimate)^2 )))
  LCI <- estimate / ci
  UCI  <- estimate * ci 
  out <- data.frame(LCI = LCI, UCI = UCI)
  return(out)
}
