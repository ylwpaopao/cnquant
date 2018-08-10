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
  if (is.null(Rb)) {
    stats <- rbind(
      table.AnnualizedReturns(Ra, scale = scale), 
      maxDrawdown(Ra)
    )
  } else {
    stats <- rbind(
      table.AnnualizedReturns(Ra, scale = scale), 
      maxDrawdown(Ra), 
      ActiveReturn(Ra, Rb, scale = scale), 
      TrackingError(Ra, Rb, scale = scale), 
      InformationRatio(Ra, Rb, scale = scale), 
      monthly_winning_rate(Ra, Rb), 
      maxDrawdown(Return.excess(Ra, Rb)) %>% `rownames<-`("Excess Return Worst Drawdown") %>% `colnames<-`(colnames(Ra))
    )
  }
  stats %>% 
    t() %>% 
    round(4L) %>% 
    return()
}
