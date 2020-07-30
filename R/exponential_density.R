exp_dens <- function(left, right, lambda){
  pleft <- pexp(left, lambda); pright <- pexp(right, lambda)
  out <- pright - pleft
  return(out)
}
