#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
uncumsum <- function(x) {
  c(x[1], diff(x))
}
