weighted_mean <- function(x, w) {
  index <- !is.na(x) & !is.na(w)
  return(weighted.mean(x[index], w[index]))
}
