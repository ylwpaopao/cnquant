#' Calculate daily price impact cost using wind tick data
#'
#' \code{calc_PIC()} calculates daily price impact cost for
#' a stock using wind tick data.
#'
#' @param tick_data A data frame.
#'
#' The tick data come from Wind.
#' This function needs the columns "code", "time", "price", "ask10", "ask9", "ask8", "ask7",
#' "ask6", "ask5", "ask4", "ask3", "ask2", "ask1", "bid1", "bid2", "bid3", "bid4", "bid5",
#' "bid6", "bid7", "bid8", "bid9", "bid10", "asize10", "asize9", "asize8", "asize7", "asize6",
#' "asize5", "asize4", "asize3", "asize2", "asize1", "bsize1", "bsize2", "bsize3", "bsize4",
#' "bsize5", "bsize6", "bsize7", "bsize8", "bsize9", "bsize10".
#'
#' @return A numeric scalar.
#' @export
#'
#' @examples
calc_PIC <- function(tick_data) {
  price_impact_cost <- tick_data %>%
    filter(between(time, 93001000L, 145659000L)) %>%
    mutate_at(vars(price, ask10:bid10), funs(. / 10 ^ 4)) %>%   # 价格单位：元
    mutate_at(vars(asize10:bsize10), funs(. / 10 ^ 6)) %>%   # 委托量单位：百万股
    mutate(cf_buy_1 = ask1 * asize1,
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

  # 考虑有些没有挂单
  price_impact_cost$cf_buy_1[price_impact_cost$ask1 == 0] <- NA
  price_impact_cost$cf_buy_2[price_impact_cost$ask2 == 0] <- NA
  price_impact_cost$cf_buy_3[price_impact_cost$ask3 == 0] <- NA
  price_impact_cost$cf_buy_4[price_impact_cost$ask4 == 0] <- NA
  price_impact_cost$cf_buy_5[price_impact_cost$ask5 == 0] <- NA
  price_impact_cost$cf_buy_6[price_impact_cost$ask6 == 0] <- NA
  price_impact_cost$cf_buy_7[price_impact_cost$ask7 == 0] <- NA
  price_impact_cost$cf_buy_8[price_impact_cost$ask8 == 0] <- NA
  price_impact_cost$cf_buy_9[price_impact_cost$ask9 == 0] <- NA
  price_impact_cost$cf_buy_10[price_impact_cost$ask10 == 0] <- NA
  price_impact_cost$cf_sell_1[price_impact_cost$bid1 == 0] <- NA
  price_impact_cost$cf_sell_2[price_impact_cost$bid2 == 0] <- NA
  price_impact_cost$cf_sell_3[price_impact_cost$bid3 == 0] <- NA
  price_impact_cost$cf_sell_4[price_impact_cost$bid4 == 0] <- NA
  price_impact_cost$cf_sell_5[price_impact_cost$bid5 == 0] <- NA
  price_impact_cost$cf_sell_6[price_impact_cost$bid6 == 0] <- NA
  price_impact_cost$cf_sell_7[price_impact_cost$bid7 == 0] <- NA
  price_impact_cost$cf_sell_8[price_impact_cost$bid8 == 0] <- NA
  price_impact_cost$cf_sell_9[price_impact_cost$bid9 == 0] <- NA
  price_impact_cost$cf_sell_10[price_impact_cost$bid10 == 0] <- NA
  price_impact_cost$pi_buy_1[price_impact_cost$ask1 == 0] <- NA
  price_impact_cost$pi_buy_2[price_impact_cost$ask2 == 0] <- NA
  price_impact_cost$pi_buy_3[price_impact_cost$ask3 == 0] <- NA
  price_impact_cost$pi_buy_4[price_impact_cost$ask4 == 0] <- NA
  price_impact_cost$pi_buy_5[price_impact_cost$ask5 == 0] <- NA
  price_impact_cost$pi_buy_6[price_impact_cost$ask6 == 0] <- NA
  price_impact_cost$pi_buy_7[price_impact_cost$ask7 == 0] <- NA
  price_impact_cost$pi_buy_8[price_impact_cost$ask8 == 0] <- NA
  price_impact_cost$pi_buy_9[price_impact_cost$ask9 == 0] <- NA
  price_impact_cost$pi_buy_10[price_impact_cost$ask10 == 0] <- NA
  price_impact_cost$pi_sell_1[price_impact_cost$bid1 == 0] <- NA
  price_impact_cost$pi_sell_2[price_impact_cost$bid2 == 0] <- NA
  price_impact_cost$pi_sell_3[price_impact_cost$bid3 == 0] <- NA
  price_impact_cost$pi_sell_4[price_impact_cost$bid4 == 0] <- NA
  price_impact_cost$pi_sell_5[price_impact_cost$bid5 == 0] <- NA
  price_impact_cost$pi_sell_6[price_impact_cost$bid6 == 0] <- NA
  price_impact_cost$pi_sell_7[price_impact_cost$bid7 == 0] <- NA
  price_impact_cost$pi_sell_8[price_impact_cost$bid8 == 0] <- NA
  price_impact_cost$pi_sell_9[price_impact_cost$bid9 == 0] <- NA
  price_impact_cost$pi_sell_10[price_impact_cost$bid10 == 0] <- NA

  # 去掉涨跌停的点
  price_impact_cost <- price_impact_cost %>%
    filter(rowSums(select(., ask10:ask1)) != 0,
           rowSums(select(., bid1:bid10)) != 0)

  price_impact_cost <- price_impact_cost %>%
    select(time, cf_buy_1:pi_sell_10) %>%
    mutate_at(vars(cf_sell_1:cf_sell_10), `-`) %>%
    mutate_at(vars(pi_buy_1:pi_sell_10), funs(. * 100)) %>%   # 价格冲击的单位变为百分数
    gather(key, value, -time) %>%
    filter(is.finite(value)) %>%
    separate(key, into = c("var", "bs", "n")) %>%
    spread(var, value)

  if (any(!is.na(price_impact_cost$cf))) {
    lm(pi ~ cf - 1, data = price_impact_cost) %>%
      coefficients() %>%
      unname() %>%
      return()
  } else {
    return(NA)
  }
}
