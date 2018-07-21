#' Calculate summary statistics
#'
#' \code{summ_stats()} calculates frequently used summary statistics for academic paper.
#'
#' @param x A numeric vector.
#' @param na.rm Logical. Should missing values (including \code{NaN}) be removed?
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' summary_stats(rnorm(100))
summ_stats <- function(x, na.rm = TRUE) {
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
