month_end <- function(x) {
  x %>%
    lubridate::ymd() %>%
    lubridate::ceiling_date("month") %>%
    `-`(lubridate::days(1)) %>%
    format("%Y%m%d") %>%
    return()
}
