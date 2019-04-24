#' Title
#'
#' @param pred
#' @param real
#' @param thre
#'
#' @return
#' @export
#'
#' @examples
relative_error <- function(pred, real, thre = Inf) {
  stopifnot(length(pred) == length(real))
  re <- abs(pred / real - 1)
  re[is.infinite(re)] <- NA_real_
  re <- pmin(re, thre)
  return(re)
}


#' Title
#'
#' @param pred
#' @param real
#' @param thre
#'
#' @return
#' @export
#'
#' @examples
mre <- function(pred, real, thre = 1) {
  re <- relative_error(pred, real, thre = thre)
  mean(re, na.rm = TRUE)
}
