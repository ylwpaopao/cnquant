#' Divide numeric into quantile groups
#'
#' \code{divide()} divides numeric into quantiles. It supplements
#' \code{cut()} by randomize repetitive values into adjacent quantiles.
#'
#' @param x A numeric vector.
#' @param n_group A positive integer (greater than or equal to 2) giving the
#' number of quantiles into which x is to be divide.
#' @param valid
#'
#' @return An integer vector with the same length of x which takes value from 1:n_group.
#' @export
#'
#' @seealso \code{\link[dplyr]{ntile}}.
#'
#' @examples
#' divide(rnorm(100))
#' divide(c(1, 2, 2, 3), n_group = 4)
divide <- function(x, n_group = 10, valid = NULL) {
  if (is.null(valid)) {
    return(dplyr::ntile(x, n_group))
  } else {
    layer <- rep(NA_integer_, length(x))
    layer[valid] <- dplyr::ntile(x[valid], n_group)
    return(layer)
  }
}
