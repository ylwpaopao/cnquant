#' Title
#'
#' @param x
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
sum_square <- function(x, na.rm = TRUE) {
  if (na.rm == TRUE)
    x <- x[!is.na(x)]
  sum(x ^ 2)
}
