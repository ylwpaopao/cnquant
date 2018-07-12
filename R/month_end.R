#' Get the last day of month
#'
#' \code{month_end()} returns the last day of the month.
#'
#' @param x A 8-digit date character.
#'
#' @return A 8-digit date character.
#' @export
#'
#' @seealso \code{\link[lubridate]{ceiling_date}}.
#'
#' @examples
#' month_end("20180712")
month_end <- function(x) {
  x %>%
    lubridate::ymd() %>%
    lubridate::ceiling_date("month") %>%
    `-`(lubridate::days(1)) %>%
    format("%Y%m%d") %>%
    return()
}
