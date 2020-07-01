# TTE Heterogeneity
# Anna Moeller 
# 2/14/2020

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