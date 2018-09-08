#' Title
#'
#' @param x
#' @param date
#' @param lag
#'
#' @return
#' @export
#'
#' @examples
lag_certain <- function(x, date, lag = "1 year") {
  date <- lubridate::ymd(date)
  lag <- strsplit(lag, " ") %>% unlist()
  date_lag <- lubridate::add_with_rollback(
    date,
    lubridate::period(-as.numeric(lag[1]), lag[2])
  )
  index <- match(date_lag, date)
  y <- x[index]
  return(y)
}
