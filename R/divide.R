#' Divide numeric into quantile groups
#'
#' \code{divide()} divides numeric into quantiles. It supplements
#' \code{cut()} by randomize repetitive values into adjacent quantiles.
#'
#' @param x A numeric vector.
#' @param n_group A positive integer (greater than or equal to 2) giving the
#' number of quantiles into which x is to be divide.
#'
#' @return An integer vector with the same length of x which takes value from 1:n_group.
#' @export
#'
#' @seealso \code{\link[base]{cut}}.
#'
#' @examples
#' divide(rnorm(100))
#' divide(c(1, 2, 2, 3), n_group = 4)
divide <- function(x, n_group = 10) {
  x <- dplyr::row_number(x)
  return(cut(
    x,
    breaks = quantile(x,
                      probs = seq(0, 1, length.out = n_group + 1),
                      na.rm = TRUE),
    include.lowest = TRUE,
    labels = FALSE
  ))
}
