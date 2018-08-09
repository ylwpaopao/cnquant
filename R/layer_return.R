#' Calculate layer returns from raw stock daily data data frame
#'
#' @param Test_Data Stock daily data data frame of certain form.
#' @param factor The factor variable.
#' @param neutrals A character vector specifying style factors (including industry) 
#' to neutralize upon. Possible components are "CITICS_IND_CODE", "LOG_S_DQ_MV", 
#' and "LOG_B2M". For example, \code{neutrals = c("CITICS_IND_CODE", "LOG_S_DQ_MV")}.
#' @param stock_pool Stock pool in which stocks are selected. "ZZALL", "HS300", or 
#' "ZZ500". Default to "ZZALL".
#' @param n_layer Number of layers. Default 10.
#' @param frequency An integer. Adjust position in every how many days?
#' @param start_point An integer. Which day from the sample beginning to firstly 
#' set up position. Usually from 1 to \code{frequency}. Default 1.
#'
#' @return An xts object of returns of each layer.
#' @export
#'
#' @examples
layer_return <- function(Test_Data, factor, neutrals = NULL, stock_pool = "ZZALL", 
                       n_layer = 10L, frequency = 20L, start_point = 1L) {
  
  # quote
  factor <- enquo(factor)
  neutrals <- syms(neutrals)
  stock_pool <- sym(stock_pool)
  
  # neutralize
  if (!is.null(neutrals)) {
    Test_Data <- Test_Data %>% 
      group_by(TRADE_DT) %>% 
      mutate(!! quo_name(factor) := neutralize(!! factor, !!! neutrals)) %>% 
      ungroup()
  }
  Test_Data <- Test_Data %>% 
    select(-LOG_S_DQ_MV, -LOG_B2M, -CITICS_IND_CODE)
  
  # filter stock_pool & update TRADABLE
  Test_Data <- Test_Data %>% 
    
    # filter stock_pool
    group_by(S_INFO_WINDCODE) %>% 
    filter(any(!! stock_pool)) %>% 
    ungroup() %>% 
    
    # update TRADABLE
    mutate(TRADABLE = TRADABLE & !! stock_pool) %>%
    select(-HS300, -ZZ500, -ZZALL)
  
  # spread Return_Data
  Return_Data <- spread_single_var(Test_Data, S_DQ_PCTCHANGE, fill = 0)
  Test_Data <- Test_Data %>% 
    select(-S_DQ_PCTCHANGE)
  
  # divide layers & assign weights
  Test_Data <- Test_Data %>% 
    
    # 筛选换仓时间
    distinct(TRADE_DT) %>% 
    slice(seq(start_point, nrow(.), by = frequency)) %>% 
    semi_join(Test_Data, .) %>% 
    
    # generate layers excluding UNTRADABLE stocks
    group_by(TRADE_DT) %>%
    mutate(LAYER = divide(!! factor, n_group = n_layer, valid = TRADABLE)) %>%   # 不可交易和因子值缺失的都为NA
    
    # W is equal weights in each layer
    group_by(TRADE_DT, LAYER) %>%
    mutate(W = 1 / n()) %>%
    ungroup() %>% 
    
    # retain necessary variables
    select(-TRADABLE, - !! factor)
  
  # calculate layer returns
  Layer_Return <- seq_len(n_layer) %>%
    lapply(function(k) {
      # 权重时间序列（调仓时间点）
      Weight_Data <- Test_Data %>%
        mutate(W = if_else(LAYER == k, W, 0)) %>%
        spread_single_var(W, fill = 0)
      
      # calculate layer returns
      Portfolio_Return <- Return.portfolio(Return_Data, Weight_Data, verbose = TRUE) %>%
        .[c("returns", "EOP.Weight")]
      
      # exclude transaction cost
      Portfolio_Return <- Portfolio_Return %>%
        .$EOP.Weight %>%
        .[seq(frequency, nrow(.), by = frequency)] %>%
        {abs(. - Weight_Data[-1, ])} %>%
        apply(1, sum) %>%
        # 去掉千分之二的交易成本
        {1 - . * 0.002} %>%
        xts(ymd(names(.))) %>%
        `names<-`("transaction.cost.discount") %>%
        merge(Portfolio_Return$returns, .) %>%
        na.fill(1) %>%
        {(1 + .[, "portfolio.returns"]) * .[, "transaction.cost.discount"] - 1} %>% 
        return()
    }) %>% 
    do.call(merge, .)
  
  names(Layer_Return) <- paste0("Layer_", seq_len(10))
  return(Layer_Return)
}
