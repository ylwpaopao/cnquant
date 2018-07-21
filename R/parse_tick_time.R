#' Parse tick time
#'
#' \code{parse_tick_time()} parses time variable from Wind tick data format.
#'
#' @param x The time character vector variable from Wind tick data format.
#'
#' @return An \code{hms} object.
#' @export
#'
#' @seealso \code{\link[readr]{parse_time}}.
#'
#' @examples
#' parse_tick_time(c("93006000", "103006000"))
parse_tick_time <- function(x) {
  x %>%
    stringr::str_sub(end = -4) %>%
    stringr::str_pad(6, pad = "0") %>%
    readr::parse_time(format = "%H%M%S") %>%
    return()
}
