mystats <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  return(c(
    mean = mean(x),
    median = median(x),
    std = sd(x),
    min = min(x),
    q = quantile(x, 0.1),
    q = quantile(x, 0.9),
    max = max(x),
    n = length(x)
  ))
}
