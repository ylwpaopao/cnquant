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
                    neutrals = NULL, stock_pool = "ZZALL") {

  # quote
  factor <- enquo(factor)
  neutrals <- syms(neutrals)
  stock_pool <- sym(stock_pool)

  # generate expected return if necessary
  if (!is.null(return_window)) {
    Test_Data <- Test_Data %>%
      group_by(S_INFO_WINDCODE) %>%
      mutate(EXPECTED_RETURN = rollapply(S_DQ_PCTCHANGE, return_window, Return.cumulative, fill = NA)) %>%
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
  if (length(neutrals) > 0) {
    Test_Data <- Test_Data %>%
      group_by(TRADE_DT) %>%
      mutate(!! quo_name(factor) := neutralize(!! factor, !!! neutrals)) %>%
      ungroup()
  }

  # stock_pool
  Test_Data <- Test_Data %>%
    filter(!! stock_pool)

  # calculate IC
  Test_Data %>%
    group_by(TRADE_DT) %>%
    summarise(IC = cor(!! factor, EXPECTED_RETURN, use = "na.or.complete"),
              Rank_IC = cor(!! factor, EXPECTED_RETURN, use = "na.or.complete", method = "spearman")) %>%
    na.omit() %>%
    return()
}
