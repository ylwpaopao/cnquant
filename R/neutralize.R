#' Neutralize one variable upon others
#'
#' @param factor The variable to neutralize.
#' @param ... Other variable to neutralize upon.
#'
#' @return The variable after neutralization. Same length of \code{factor}.
#' @export
#'
#' @examples
neutralize <- function(factor, ...) {
  df <- tibble::tibble(factor, ...)
  lm(factor ~ ., data = df, na.action = na.exclude) %>% 
    residuals() %>% 
    return()
}
