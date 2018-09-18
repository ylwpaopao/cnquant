#' Title
#'
#' @param .data1
#' @param .data2
#' @param stock_code
#' @param trade_date
#' @param ENTRY_DT
#' @param REMOVE_DT
#' @param industry
#'
#' @return
#' @export
#'
#' @examples
match_industry <- function(.data1, .data2, stock_code, trade_date, ENTRY_DT, REMOVE_DT, industry) {
  stock_code <- enquo(stock_code)
  trade_date <- enquo(trade_date)
  ENTRY_DT <- enquo(ENTRY_DT)
  REMOVE_DT <- enquo(REMOVE_DT)
  industry <- enquo(industry)

  .data1 %>%
    select(!! stock_code, !! trade_date) %>%
    left_join(.data2, by = quo_name(stock_code)) %>%
    mutate(FLAG = !! trade_date >= ENTRY_DT & !! trade_date <= REMOVE_DT) %>%
    filter(FLAG) %>%
    select(!! stock_code, !! trade_date, !! industry) %>%
    left_join(.data1, ., by = c(quo_name(stock_code), quo_name(trade_date))) %>%
    return()
}
