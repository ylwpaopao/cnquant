# snippet function
# monthly winning rate against benchmark
monthly_winning_rate <- function(Ra, Rb) {
  merge(Ra, Rb) %>%
    as_tibble() %>%
    tibble::rownames_to_column("TRADE_DT") %>%
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
#' @param lang
#'
#' @return A statistic matrix.
#' @export
#'
#' @examples
portf_stats <- function(Ra, Rb = Ra[, ncol(Ra)], scale = NA, lang = "CHN") {
  # functions in PerformanceAnalytics return a 1-row matrix
  # basic stats
  stats1 <- rbind(
    PerformanceAnalytics::Return.annualized(Ra, scale = scale),
    PerformanceAnalytics::StdDev.annualized(Ra, scale = scale),
    PerformanceAnalytics::SharpeRatio.annualized(Ra, scale = scale),
    PerformanceAnalytics::maxDrawdown(Ra)
  )
  if (lang == "CHN")
    rownames(stats1) <- c("年化收益率", "年化波动率", "年化夏普比", "最大回撤")

  # additional stats requiring benchmark returns
  if (!is.null(Rb)) {
    stats2 <- rbind(
      PerformanceAnalytics::ActiveReturn(Ra, Rb, scale = scale),
      PerformanceAnalytics::TrackingError(Ra, Rb, scale = scale),
      PerformanceAnalytics::InformationRatio(Ra, Rb, scale = scale),
      monthly_winning_rate(Ra, Rb),
      PerformanceAnalytics::maxDrawdown(Return.excess(Ra, Rb)) %>% `rownames<-`("Excess Return Worst Drawdown") %>% `colnames<-`(colnames(Ra))
    )
    if (lang == "CHN")
      rownames(stats2) <- c("超额年化收益率", "年化跟踪误差", "信息比", "相对基准月胜率", "超额收益最大回撤")
    stats <- rbind(stats1, stats2)
  }

  # adjust and return
  stats %>%
    t() %>%
    as_tibble(rownames = if_else(lang == "CHN", "投资组合", "Portfolio")) %>%
    return()
}
