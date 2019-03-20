#' Move parallel a matrix and pad NAs
#'
#' Simple version. Currently only "right" direction is supported.
#'
#' @param data A matrix.
#' @param direction
#' @param n
#'
#' @return
#' @export
#'
#' @examples
move_para <- function(data, direction = "right", n = 1L) {
  data <- data[, 1:(ncol(data) - n)]
  pad <- matrix(NA, nrow = nrow(data), ncol = n)
  data <- cbind(pad, data)
  return(data)
}
