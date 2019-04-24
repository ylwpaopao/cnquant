#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
na.fill_mod <- function(x) {
  if (sum(!is.na(x)) == 0L) {
    return(x)
  }
  else if (sum(!is.na(x)) == 1L) {
    x[1:length(x)] <- x[!is.na(x)]
    return(x)
  }
  else {
    x <- zoo::na.fill(x, "extend")
    return(x)
  }
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
na.fill_mean <- function(x) {
  if (sum(!is.na(x)) == 0L) {
    return(x)
  }
  else {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    return(x)
  }
}
