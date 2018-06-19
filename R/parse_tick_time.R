parse_tick_time <- function(x) {
  x %>%
    stringr::str_sub(end = -4) %>%
    stringr::str_pad(6, pad = "0") %>%
    readr::parse_time(format = "%H%M%S") %>%
    return()
}
