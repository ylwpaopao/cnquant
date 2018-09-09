#' Title
#'
#' @param Test_Data
#' @param factor
#' @param return_window A list. Refer to width in rollapply.
#' @param standarize
#' @param neutrals
#' @param stock_pool
#'
#' @return
#' @export
#'
#' @examples
calc_IC <- function(Test_Data, factor, return_window = NULL, standarize = FALSE,
                    neutrals = NULL, stock_pool = "NONEST") {

  # quote
  factor <- enquo(factor)

  # generate expected return if necessary
  # if is.null(return_window), Expected_Return must exist before, it's usually for testing
  if (!is.null(return_window)) {
    Test_Data <- Test_Data %>%
      group_by(S_INFO_WINDCODE) %>%
      mutate(Expected_Return = zoo::rollapply(S_DQ_PCTCHANGE, return_window, PerformanceAnalytics::Return.cumulative, fill = NA)) %>%
      ungroup()
  }

  # standarize
  if (standarize) {
    Test_Data <- Test_Data %>%
      group_by(TRADE_DT) %>%
      mutate(!! quo_name(factor) := scale(!! factor)) %>%
      ungroup()
  }

  # neutralize
  if (!is.null(neutrals)) {
    neutrals <- syms(neutrals)

    Test_Data <- Test_Data %>%
      group_by(TRADE_DT) %>%
      mutate(!! quo_name(factor) := neutralize(!! factor, !!! neutrals)) %>%
      ungroup()
  }

  # stock_pool
  if (stock_pool != "NONEST") {
    stock_pool <- sym(stock_pool)
    Test_Data <- Test_Data %>%
      filter(!! stock_pool)
  }

  # calculate IC
  Test_Data %>%
    group_by(TRADE_DT) %>%
    summarise(IC = cor(!! factor, Expected_Return, use = "na.or.complete"),
              Rank_IC = cor(!! factor, Expected_Return, use = "na.or.complete", method = "spearman")) %>%
    na.omit() %>%
    return()
}
