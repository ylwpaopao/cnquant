# snippet function
# monthly winning rate against benchmark
monthly_winning_rate <- function(Ra, Rb) {
  merge(Ra, Rb) %>%
    as_tibble() %>%
    rownames_to_column("TRADE_DT") %>%
    mutate(TRADE_DT = substr(TRADE_DT, 1, 7)) %>%
    group_by(TRADE_DT) %>%
    summarise_if(is.numeric, Return.cumulative) %>%
    mutate_if(is.numeric, `-`, .[[ncol(.)]]) %>%
    select(-ncol(.)) %>%
    summarise_if(is.numeric, funs(mean(. > 0))) %>%
    as.matrix() %>%
    `rownames<-`("Monthly Winning Rate") %>%
    return()
}


#' Summary statistics for portfolio(s)
#'
#' @param Ra An xts object of portfolio return.
#' @param Rb An xts object of benchmark return.
#' @param scale Number of period per year.
#'
#' @return A statistic matrix.
#' @export
#'
#' @examples
portf_stats <- function(Ra, Rb = NULL, scale = NA) {
  stats <- rbind(
    Return.annualized(Ra, scale = scale),
    StdDev.annualized(Ra, scale = scale),
    SharpeRatio.annualized(Ra, scale = scale),
    maxDrawdown(Ra)
  ) %>%
    `rownames<-`(c("年化收益率", "年化波动率", "年化夏普比", "最大回撤"))

  if (!is.null(Rb)) {
    stats <- rbind(
      stats,
      rbind(
        "超额年化收益率" = ActiveReturn(Ra, Rb, scale = scale),
        "年化跟踪误差" = TrackingError(Ra, Rb, scale = scale),
        "信息比" = InformationRatio(Ra, Rb, scale = scale),
        "相对基准月胜率" = monthly_winning_rate(Ra, Rb),
        "超额收益最大回撤" = maxDrawdown(Return.excess(Ra, Rb)) %>% `rownames<-`("Excess Return Worst Drawdown") %>% `colnames<-`(colnames(Ra))
      ) %>%
        `rownames<-`(c("超额年化收益率", "年化跟踪误差", "信息比", "相对基准月胜率", "超额收益最大回撤"))
    )
  }
  stats %>%
    t() %>%
    round(4L) %>%
    return()
}
