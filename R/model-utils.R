#' Mean absolute error
#'
#' Mean absolute error of 2 vectors
#'
#' @param x
#' @param y
#' @param na.rm
#'
#' @return A numeric scalar.
#' @export
#'
#' @examples
MAE <- function(x, y, na.rm = TRUE) {
  diff <- x - y
  if (na.rm == TRUE)
    diff <- diff[!is.na(diff)]
  diff %>%
    abs() %>%
    mean() %>%
    return()
}

#' Root of mean square error
#'
#' Root of mean square error of 2 vectors
#'
#' @param x
#' @param y
#' @param na.rm
#'
#' @return A numeric scalar.
#' @export
#'
#' @examples
RMSE <- function(x, y, na.rm = TRUE) {
  diff <- x - y
  if (na.rm == TRUE)
    diff <- diff[!is.na(diff)]
  sdiff <- diff ^ 2
  sdiff %>%
    mean() %>%
    sqrt() %>%
    return()
}
