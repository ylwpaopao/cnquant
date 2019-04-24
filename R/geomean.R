#' Title
#'
#' @param x
#' @param neg.rm
#' @param default
#'
#' @return
#' @export
#'
#' @examples
geomean <- function(x, neg.rm = TRUE, default = 1) {
  x <- x[is.finite(x)]
  if (neg.rm) x <- x[x > 0]
  if (length(x) == 0L) return(default)
  else return(prod(x) ^ (1 / length(x)))
}
