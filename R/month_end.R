#' Get the last day of month
#'
#' \code{month_end()} returns the last day of the month.
#'
#' @param x A \code{Date} object or a 8-digit date character.
#'
#' @return The same type of x.
#' @export
#'
#' @seealso \code{\link[lubridate]{ceiling_date}}.
#'
#' @examples
#' month_end("20180712")
#' month_end(as.Date("2018-07-12"))
month_end <- function(x) {
  if (inherits(x, "Date")) {
    x %>%
      lubridate::ceiling_date("month") %>%
      `-`(lubridate::days(1)) %>%
      return()
  } else if (is.character(x)) {
    x %>%
      lubridate::ymd() %>%
      month_end() %>%
      format("%Y%m%d") %>%
      return()
  }
}
