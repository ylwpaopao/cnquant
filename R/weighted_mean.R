#' Weighted arithmetic mean
#'
#' \code{weighted_mean()} computes a weighted arithmetic mean. It enhances the
#' \code{weighted.mean} function with support for \code{NA} values in \code{x}. In
#' fact, \code{weighted_mean()} excludes element-pair with \coed{NA} either from \coed{x} or
#' \coed{w}.
#'
#' @param x A numeric vector.
#' @param w A numeric vector of weights the same length as \code{x} giving the
#' weights.
#'
#' @return A numeric scalar.
#' @export
#'
#' @examples
#' x1 <- c(NA, 2, 3, 4, 5)
#' w1 <- c(5, NA, 3, 2, 1)
#' weighted_mean(x1, w1)
#'
#' x2 <- x1[3:5]
#' w2 <- w1[3:5]
#' weighted_mean(x2, w2)
weighted_mean <- function(x, w) {
  index <- !is.na(x) & !is.na(w)
  return(weighted.mean(x[index], w[index]))
}
