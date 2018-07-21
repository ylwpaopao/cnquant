#' Replace a certain value to another
#'
#' \code{replace_certain()} replaces a certain value in a vector to another.
#' \code{NaN} and \code{NA} are also supported.
#'
#' @param x A vector.
#' @param pattern A certain value to be replaced.
#' @param replacement A certain value to replace to.
#'
#' @return The modified vector.
#' @export
#'
#' @examples
#' replace_certain(letters, "a", "b")
#' replace_certain(c(NaN, NA, 3), NaN, 1)
#' replace_certain(c(NaN, NA, 3), NA, 1)
replace_certain <- function(x, pattern, replacement) {
  if (is.nan(pattern)) x[is.nan(x)] <- replacement
  else if (is.na(pattern)) x[is.na(x)] <- replacement
  else x[x == pattern] <- replacement
  return(x)
}
