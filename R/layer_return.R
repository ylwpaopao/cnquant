#' Calculate layer returns from raw stock daily data data frame
#'
#' @param Test_Data Stock daily data data frame of certain form.
#' @param factor The factor variable.
#' @param neutrals A character vector specifying style factors (including industry)
#' to neutralize upon. Possible components are "CITICS_IND_CODE", "LOG_S_DQ_MV",
#' and "LOG_B2M". For example, \code{neutrals = c("CITICS_IND_CODE", "LOG_S_DQ_MV")}.
#' @param stock_pool Stock pool in which stocks are selected. "NONEST", "ZZALL", "HS300", or
#' "ZZ500". Default to "NONEST".
#' @param n_layer Number of layers. Default 10.
#' @param frequency An integer. Adjust position in every how many days?
#' @param start_point An integer. Which day from the sample beginning to firstly
#' set up position. Usually from 1 to \code{frequency}. Default 1.
#' @param lang
#'
#' @return An xts object of returns of each layer.
#' @export
#'
#' @examples
layer_return <- function(Test_Data, factor, neutrals = NULL, stock_pool = "NONEST",
                       n_layer = 10L, frequency = 20L, start_point = 1L, lang = "CHN") {

  # quote
  factor <- enquo(factor)

  # neutralize
  if (!is.null(neutrals)) {
    neutrals <- syms(neutrals)

    Test_Data <- Test_Data %>%
      group_by(TRADE_DT) %>%
      mutate(!! quo_name(factor) := neutralize(!! factor, !!! neutrals)) %>%
      ungroup()
  }

  # filter stock_pool & update TRADABLE
  if (stock_pool != "NONEST") {
    stock_pool <- sym(stock_pool)

    Test_Data <- Test_Data %>%

      # filter stock_pool
      group_by(S_INFO_WINDCODE) %>%
      filter(any(!! stock_pool)) %>%
      ungroup() %>%

      # update TRADABLE
      mutate(TRADABLE = TRADABLE & !! stock_pool)
  }

  # spread Return_Data
  Return_Data <- spread_single_var(Test_Data, S_DQ_PCTCHANGE, fill = 0)

  # divide layers & assign weights
  Test_Data <- Test_Data %>%

    # 筛选换仓时间
    distinct(TRADE_DT) %>%
    slice(seq(start_point, nrow(.), by = frequency)) %>%
    semi_join(Test_Data, ., by = "TRADE_DT") %>%
    tidyr::complete(S_INFO_WINDCODE = unique(Test_Data$S_INFO_WINDCODE),
             fill = list(TRADE_DT = last(.$TRADE_DT), TRADABLE = FALSE)) %>%

    # generate layers and benchmark weights excluding UNTRADABLE stocks
    group_by(TRADE_DT) %>%
    mutate(LAYER = divide(!! factor, n_group = n_layer, valid = TRADABLE),   # 不可交易和因子值缺失的都为NA
           # Benchmark为所有TRADABLE股票的等权组合
           W_Benchmark = if_else(TRADABLE & !is.na(!! factor), 1 / sum(TRADABLE & !is.na(!! factor)), 0)) %>%

    # W is equal weights in each layer
    group_by(TRADE_DT, LAYER) %>%
    mutate(W = 1 / n()) %>%
    ungroup()

  # calculate layer returns
  Layer_Return <- seq_len(n_layer) %>%
    lapply(function(k) {
      # 权重时间序列（调仓时间点）
      Weight_Data <- Test_Data %>%
        mutate(W = if_else(LAYER == k, W, 0)) %>%
        spread_single_var(W, fill = 0)

      # calculate each layer return
      portfolio_return_excost(Return_Data, Weight_Data) %>%
        return()
    }) %>%
    do.call(merge, .)
  names(Layer_Return) <- paste0("Layer_", seq_len(n_layer))

  # 加入多空组合
  Layer_Return <- Layer_Return %>%
    {.[, 1] - .[, n_layer]} %>%
    `names<-`("Long_Short") %>%
    merge(Layer_Return, .)

  # 加入基准组合
  Layer_Return <- Test_Data %>%
    spread_single_var(W_Benchmark, fill = 0) %>%
    portfolio_return_excost(Return_Data, .) %>%
    `names<-`("Benchmark") %>%
    merge(Layer_Return, .)

  # 组合名称语言
  if (lang == "CHN") {
    names(Layer_Return) <- c(paste("分层组合", seq_len(n_layer)), "多空组合", "基准组合")
  }

  return(Layer_Return)
}
