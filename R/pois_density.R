pois_dens <- function(x, lambda){
  out <- lambda^x * exp(-lambda) / factorial(x)
}