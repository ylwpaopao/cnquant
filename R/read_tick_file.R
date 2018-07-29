read_tick_file <- function(file){

  Tick_Data <- readr::read_csv(file)

  ## 因为在后面的运行中发现有的天是缺少某些变量的，所以这里check一下
  Variables_Names <- names(Tick_Data)
  Check_Variables_Exist <- c("code", "time", "price", "ask10", "ask9", "ask8", "ask7",
                             "ask6", "ask5", "ask4", "ask3", "ask2", "ask1", "bid1",
                             "bid2", "bid3", "bid4", "bid5", "bid6", "bid7", "bid8",
                             "bid9", "bid10", "asize10", "asize9", "asize8", "asize7",
                             "asize6", "asize5", "asize4", "asize3", "asize2", "asize1",
                             "bsize1", "bsize2", "bsize3", "bsize4", "bsize5", "bsize6",
                             "bsize7", "bsize8", "bsize9", "bsize10") %in% Variables_Names

  ## 计算回归结果
  ## 如果需要的变量都有的话就运行计算的程序，否则就输出一个带日期带股票代码的全是NA的
  if(all(Check_Variables_Exist) & (nrow(Tick_Data) > 1000)){
    ## 个股数据的处理
    ## 取时间段为9：30：01—14：56：59
    stkcode <- Tick_Data$code[1]

    coef_SH <- Tick_Data %>%
      dplyr::filter(time >= 93001000 & time <= 145659000) %>%
      dplyr::select(time, price, ask10, ask9, ask8, ask7, ask6, ask5, ask4, ask3, ask2, ask1,
             bid1, bid2, bid3, bid4, bid5, bid6, bid7, bid8, bid9, bid10,
             asize10, asize9, asize8, asize7, asize6, asize5, asize4, asize3, asize2,
             asize1, bsize1, bsize2, bsize3, bsize4, bsize5, bsize6, bsize7, bsize8,
             bsize9, bsize10) %>%
      dplyr::group_by(time) %>%
      dplyr::slice(n()) %>%
      dplyr::ungroup()

    coef_SH <- coef_SH %>%
      dplyr::mutate_at(vars(price, ask10:bid10), funs(. / 10 ^ 4)) %>%   # 价格单位：元
      dplyr::mutate_at(vars(asize10:bsize10), funs(. / 10 ^ 6)) %>%   # 委托量单位：百万股
      dplyr::mutate(cf_buy_1 = ask1 * asize1,
             cf_buy_2 = cf_buy_1 + ask2 * asize2,
             cf_buy_3 = cf_buy_2 + ask3 * asize3,
             cf_buy_4 = cf_buy_3 + ask4 * asize4,
             cf_buy_5 = cf_buy_4 + ask5 * asize5,
             cf_buy_6 = cf_buy_5 + ask6 * asize6,
             cf_buy_7 = cf_buy_6 + ask7 * asize7,
             cf_buy_8 = cf_buy_7 + ask8 * asize8,
             cf_buy_9 = cf_buy_8 + ask9 * asize9,
             cf_buy_10 = cf_buy_9 + ask10 * asize10,
             cf_sell_1 = bid1 * bsize1,
             cf_sell_2 = cf_sell_1 + bid2 * bsize2,
             cf_sell_3 = cf_sell_2 + bid3 * bsize3,
             cf_sell_4 = cf_sell_3 + bid4 * bsize4,
             cf_sell_5 = cf_sell_4 + bid5 * bsize5,
             cf_sell_6 = cf_sell_5 + bid6 * bsize6,
             cf_sell_7 = cf_sell_6 + bid7 * bsize7,
             cf_sell_8 = cf_sell_7 + bid8 * bsize8,
             cf_sell_9 = cf_sell_8 + bid9 * bsize9,
             cf_sell_10 = cf_sell_9 + bid10 * bsize10,
             pi_buy_1 = (ask1 - price) / price,
             pi_buy_2 = (ask2 - price) / price,
             pi_buy_3 = (ask3 - price) / price,
             pi_buy_4 = (ask4 - price) / price,
             pi_buy_5 = (ask5 - price) / price,
             pi_buy_6 = (ask6 - price) / price,
             pi_buy_7 = (ask7 - price) / price,
             pi_buy_8 = (ask8 - price) / price,
             pi_buy_9 = (ask9 - price) / price,
             pi_buy_10 = (ask10 - price) / price,
             pi_sell_1 = (bid1 - price) / price,
             pi_sell_2 = (bid2 - price) / price,
             pi_sell_3 = (bid3 - price) / price,
             pi_sell_4 = (bid4 - price) / price,
             pi_sell_5 = (bid5 - price) / price,
             pi_sell_6 = (bid6 - price) / price,
             pi_sell_7 = (bid7 - price) / price,
             pi_sell_8 = (bid8 - price) / price,
             pi_sell_9 = (bid9 - price) / price,
             pi_sell_10 = (bid10 - price) / price)

    ## 考虑有些没有挂单
    coef_SH$cf_buy_1[coef_SH$ask1 == 0] <- NA
    coef_SH$cf_buy_2[coef_SH$ask2 == 0] <- NA
    coef_SH$cf_buy_3[coef_SH$ask3 == 0] <- NA
    coef_SH$cf_buy_4[coef_SH$ask4 == 0] <- NA
    coef_SH$cf_buy_5[coef_SH$ask5 == 0] <- NA
    coef_SH$cf_buy_6[coef_SH$ask6 == 0] <- NA
    coef_SH$cf_buy_7[coef_SH$ask7 == 0] <- NA
    coef_SH$cf_buy_8[coef_SH$ask8 == 0] <- NA
    coef_SH$cf_buy_9[coef_SH$ask9 == 0] <- NA
    coef_SH$cf_buy_10[coef_SH$ask10 == 0] <- NA
    coef_SH$cf_sell_1[coef_SH$bid1 == 0] <- NA
    coef_SH$cf_sell_2[coef_SH$bid2 == 0] <- NA
    coef_SH$cf_sell_3[coef_SH$bid3 == 0] <- NA
    coef_SH$cf_sell_4[coef_SH$bid4 == 0] <- NA
    coef_SH$cf_sell_5[coef_SH$bid5 == 0] <- NA
    coef_SH$cf_sell_6[coef_SH$bid6 == 0] <- NA
    coef_SH$cf_sell_7[coef_SH$bid7 == 0] <- NA
    coef_SH$cf_sell_8[coef_SH$bid8 == 0] <- NA
    coef_SH$cf_sell_9[coef_SH$bid9 == 0] <- NA
    coef_SH$cf_sell_10[coef_SH$bid10 == 0] <- NA
    coef_SH$pi_buy_1[coef_SH$ask1 == 0] <- NA
    coef_SH$pi_buy_2[coef_SH$ask2 == 0] <- NA
    coef_SH$pi_buy_3[coef_SH$ask3 == 0] <- NA
    coef_SH$pi_buy_4[coef_SH$ask4 == 0] <- NA
    coef_SH$pi_buy_5[coef_SH$ask5 == 0] <- NA
    coef_SH$pi_buy_6[coef_SH$ask6 == 0] <- NA
    coef_SH$pi_buy_7[coef_SH$ask7 == 0] <- NA
    coef_SH$pi_buy_8[coef_SH$ask8 == 0] <- NA
    coef_SH$pi_buy_9[coef_SH$ask9 == 0] <- NA
    coef_SH$pi_buy_10[coef_SH$ask10 == 0] <- NA
    coef_SH$pi_sell_1[coef_SH$bid1 == 0] <- NA
    coef_SH$pi_sell_2[coef_SH$bid2 == 0] <- NA
    coef_SH$pi_sell_3[coef_SH$bid3 == 0] <- NA
    coef_SH$pi_sell_4[coef_SH$bid4 == 0] <- NA
    coef_SH$pi_sell_5[coef_SH$bid5 == 0] <- NA
    coef_SH$pi_sell_6[coef_SH$bid6 == 0] <- NA
    coef_SH$pi_sell_7[coef_SH$bid7 == 0] <- NA
    coef_SH$pi_sell_8[coef_SH$bid8 == 0] <- NA
    coef_SH$pi_sell_9[coef_SH$bid9 == 0] <- NA
    coef_SH$pi_sell_10[coef_SH$bid10 == 0] <- NA


    coef_SH <- coef_SH %>%
      dplyr::select(time,
             cf_buy_1, cf_buy_2, cf_buy_3, cf_buy_4, cf_buy_5,
             cf_buy_6, cf_buy_7, cf_buy_8, cf_buy_9, cf_buy_10,
             cf_sell_1, cf_sell_2, cf_sell_3, cf_sell_4, cf_sell_5,
             cf_sell_6, cf_sell_7, cf_sell_8, cf_sell_9, cf_sell_10,
             pi_buy_1, pi_buy_2, pi_buy_3, pi_buy_4, pi_buy_5,
             pi_buy_6, pi_buy_7, pi_buy_8, pi_buy_9, pi_buy_10,
             pi_sell_1, pi_sell_2, pi_sell_3, pi_sell_4, pi_sell_5,
             pi_sell_6, pi_sell_7, pi_sell_8, pi_sell_9, pi_sell_10) %>%
      dplyr::mutate_at(vars(cf_sell_1:cf_sell_10), `-`) %>%
      dplyr::mutate_at(vars(pi_buy_1:pi_sell_10), funs(. * 100)) %>%   # 价格冲击的单位变为百分数
      gather(key, value, -time) %>%
      dplyr::filter(!is.na(value), !is.nan(value), !is.infinite(value)) %>%
      dplyr::separate(key, into = c("var", "bs", "n")) %>%
      dplyr::spread(var, value)

    reg <- summary(lm(pi ~ cf - 1, data = coef_SH))
    slope <- reg$coefficients[1,1]

    coef_SH <- slope

  }else{
    coef_SH <- NA
  }



}
