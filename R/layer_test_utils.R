# snippet function
# regular cleaning mission
clean_stock_daily_data <- function(Stock_Daily_Data) {
  Stock_Daily_Data %>% 
    
    # S_DQ_PCTCHANGE
    mutate(S_DQ_PCTCHANGE = S_DQ_PCTCHANGE / 100, 
           S_DQ_PCTCHANGE = winsorize(S_DQ_PCTCHANGE, 
                                      method = c(-0.105, 0.103), fill = "NA")) %>% 
    replace_na(list(S_DQ_PCTCHANGE = 0)) %>% 
    
    # S_DQ_MV
    mutate(S_DQ_MV = log(S_DQ_MV)) %>% 
    rename(LOG_S_DQ_MV = S_DQ_MV) %>% 
    
    # S_VAL_PB_NEW
    mutate(S_VAL_PB_NEW = log(1 / S_VAL_PB_NEW)) %>% 
    rename(LOG_B2M = S_VAL_PB_NEW) %>% 
    mutate(LOG_B2M = winsorize(LOG_B2M, method = "quantile", side = "left")) %>% 
    
    # TRADABLE
    replace_na(list(UP_DOWN_LIMIT_STATUS = 0)) %>% 
    mutate(TRADABLE = S_DQ_TRADESTATUS %in% c("交易", "DR", "XD", "XR") 
           & UP_DOWN_LIMIT_STATUS == 0 
           & !ST) %>% 
    select(-S_DQ_TRADESTATUS, -UP_DOWN_LIMIT_STATUS, -ST) %>% 
    
    return()
}


# snippet function
# spread var along stock code
spread_single_var <- function(Test_Data, var, fill = NA) {
  var <- enquo(var)
  Test_Data %>%
    select(S_INFO_WINDCODE, TRADE_DT, !! var) %>%
    spread(S_INFO_WINDCODE, !! var, fill = fill) %>%
    {xts(select(., -TRADE_DT), ymd(.$TRADE_DT))} %>% 
    return()
}
