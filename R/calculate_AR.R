#' Calculate abnormal return by factor model
#'
#' \code{calculate_AR()} calculates abnormal return by factor model.
#'
#' @param data A data frame.
#'
#' The first column is the trading date whose class is "Date".
#' The second column is the return.
#' The others are factors.
#' @param estimation_window_start
#' A positive integer giving the start time point of beta estimation window.
#' @param estimation_window_end
#' A positive integer giving the end time point of beta estimation window.
#' @param least_valid_days
#' A positive integer giving the least vaild trading days in the estimation window.
#' @param estimation_window_start_type
#' A character.
#'
#' "day" means the start time point is the nunmber of days.
#' "month" means the start time point is the number of months.
#' "year" means the start time point is the number of years.
#' @param estimation_window_end_type
#' A character.
#'
#' "day" means the end time point is the number of days.
#' "month" means the end time point is the number of months.
#' "year" means the end time point is the number of years.
#' @param factor_number
#' A positive integer.
#'
#' 1 means there is 1 factor.
#' 3 means there are 3 factors.
#' 4 means there are 4 factors.
#' 5 means there are 5 factors.
#'
#'
#' @return A double vector.
#' @export
#'
#' @examples
calculate_AR <- function(data,
                         estimation_window_start,
                         estimation_window_end,
                         least_valid_days,
                         estimation_window_start_type = "day",
                         estimation_window_end_type = "day",
                         factor_number = 3L){
  ## trade_date: 交易日期
  ## R: 收益率
  ## factors: 选择放入几个因子
  ## estimation_window_start: 估计beta的日历日时间窗口开始时间
  ## estimation_window_end: 估计beta的日历日时间窗口结束时间
  ## least_valid_days: 在估计的时间窗口中有效交易日高于多少时才计算
  ## estimation_window_start_type: == "day"表示时间窗口的起始点给的是多少天；
  ##                               == "month"表示时间窗口的起始点给的是多少月；
  ##                               == "year"表示时间窗口的起始点给的是多少年
  ## estimation_window_end_type:   == "day"表示时间窗口的终止点给的是多少天；
  ##                               == "month"表示时间窗口的终止点给的是多少月；
  ##                               == "year"表示时间窗口的终止点给的是多少年
  ## factor_number: == 1L 表示用1因子模型
  ##                == 3L 表示用3因子模型
  ##                == 4L 表示用4因子模型
  ##                == 5L 表示用5因子模型


  if(factor_number == 3L){
    ## 将data里面变量的名字改为trade_date, R, factor1, factor2, factor3
    names(data) <- c("trade_date", "R", "factor1", "factor2", "factor3")

    ## 根据estimation_window_start和estimation_window_end生成估计beta系数的估计窗口
    ## 生成估计窗口的起始点
    if(estimation_window_start_type == "day"){
      data$estimation_start_date = data$trade_date - lubridate::days(estimation_window_start)
    }else if(estimation_window_start_type == "month"){
      data$estimation_start_date = lubridate::`%m-%`(data$trade_date,
                                                     months(estimation_window_start))
    }else if(estimation_window_start_type == "year"){
      data$estimation_start_date = lubridate::`%m-%`(data$trade_date,
                                                     lubridate::years(estimation_window_start))
    }

    ## 生成估计窗口的终止点
    if(estimation_window_end_type == "day"){
      data$estimation_end_date = data$trade_date - lubridate::days(estimation_window_end)
    }else if(estimation_window_end_type == "month"){
      data$estimation_end_date = lubridate::`%m-%`(data$trade_date,
                                                   months(estimation_window_end))
    }else if(estimation_window_end_type == "year"){
      data$estimation_end_date = lubridate::`%m-%`(data$trade_date,
                                                   lubridate::years(estimation_window_end))
    }

    ## 生成估计窗口中有效交易日的个数
    data$available_trade_days_number =
      sapply(1 : nrow(data), function(i){
        sum((data$trade_date >= data$estimation_start_date[i]) &
              (data$trade_date <= data$estimation_end_date[i]))})


    ## 根据上面的data.frame结构计算超额收益率的函数
    CalculateAR <- function(AR_data, i){

      if(AR_data$available_trade_days_number[i] >= least_valid_days){

        in_window <- (AR_data$trade_date >= AR_data$estimation_start_date[i]) &
          (AR_data$trade_date <= AR_data$estimation_end_date[i])

        R.sub <- AR_data$R[in_window]
        factor1.sub <- data$factor1[in_window]
        factor2.sub <- data$factor2[in_window]
        factor3.sub <- data$factor3[in_window]

        reg <- lm(R.sub ~ factor1.sub + factor2.sub + factor3.sub)
        intercept <- coef(reg)[1]
        coef_factor1 <- coef(reg)[2]
        coef_factor2 <- coef(reg)[3]
        coef_factor3 <- coef(reg)[4]
        ar <- AR_data$R[i] - (intercept + coef_factor1 * AR_data$factor1[i] +
                                coef_factor2 * AR_data$factor2[i] +
                                coef_factor3 * AR_data$factor3[i])

        return(ar)
      }else{
        return(NA)
      }
    }

    ## 生成超额收益率ar那一列
    ar = sapply(1 : nrow(data), function(i){CalculateAR(data, i)})


    return(ar)

  }else if(factor_number == 4L){
    ## 将data里面变量的名字改为trade_date, R, factor1, factor2, factor3, factor4
    names(data) <- c("trade_date", "R", "factor1", "factor2", "factor3", "factor4")

    ## 根据estimation_window_start和estimation_window_end生成估计beta系数的估计窗口
    ## 生成估计窗口的起始点
    if(estimation_window_start_type == "day"){
      data$estimation_start_date = data$trade_date - lubridate::days(estimation_window_start)
    }else if(estimation_window_start_type == "month"){
      data$estimation_start_date = lubridate::`%m-%`(data$trade_date,
                                                     months(estimation_window_start))
    }else if(estimation_window_start_type == "year"){
      data$estimation_start_date = lubridate::`%m-%`(data$trade_date,
                                                     lubridate::years(estimation_window_start))
    }

    ## 生成估计窗口的终止点
    if(estimation_window_end_type == "day"){
      data$estimation_end_date = data$trade_date - lubridate::days(estimation_window_end)
    }else if(estimation_window_end_type == "month"){
      data$estimation_end_date = lubridate::`%m-%`(data$trade_date,
                                                   months(estimation_window_end))
    }else if(estimation_window_end_type == "year"){
      data$estimation_end_date = lubridate::`%m-%`(data$trade_date,
                                                   lubridate::years(estimation_window_end))
    }

    ## 生成估计窗口中有效交易日的个数
    data$available_trade_days_number =
      sapply(1 : nrow(data), function(i){
        sum((data$trade_date >= data$estimation_start_date[i]) &
              (data$trade_date <= data$estimation_end_date[i]))})


    ## 根据上面的data.frame结构计算超额收益率的函数
    CalculateAR <- function(AR_data, i){

      if(AR_data$available_trade_days_number[i] >= least_valid_days){

        in_window <- (AR_data$trade_date >= AR_data$estimation_start_date[i]) &
          (AR_data$trade_date <= AR_data$estimation_end_date[i])

        R.sub <- AR_data$R[in_window]
        factor1.sub <- data$factor1[in_window]
        factor2.sub <- data$factor2[in_window]
        factor3.sub <- data$factor3[in_window]
        factor4.sub <- data$factor4[in_window]

        reg <- lm(R.sub ~ factor1.sub + factor2.sub + factor3.sub + factor4.sub)
        intercept <- coef(reg)[1]
        coef_factor1 <- coef(reg)[2]
        coef_factor2 <- coef(reg)[3]
        coef_factor3 <- coef(reg)[4]
        coef_factor4 <- coef(reg)[5]
        ar <- AR_data$R[i] - (intercept + coef_factor1 * AR_data$factor1[i] +
                                coef_factor2 * AR_data$factor2[i] +
                                coef_factor3 * AR_data$factor3[i] +
                                coef_factor4 * AR_data$factor4[i])

        return(ar)
      }else{
        return(NA)
      }
    }

    ## 生成超额收益率ar那一列
    ar = sapply(1 : nrow(data), function(i){CalculateAR(data, i)})


    return(ar)

  }else if(factor_number == 5L){
    ## 将data里面变量的名字改为trade_date, R, factor1, factor2, factor3, factor4, factor5
    names(data) <- c("trade_date", "R", "factor1", "factor2", "factor3", "factor4", "factor5")

    ## 根据estimation_window_start和estimation_window_end生成估计beta系数的估计窗口
    ## 生成估计窗口的起始点
    if(estimation_window_start_type == "day"){
      data$estimation_start_date = data$trade_date - lubridate::days(estimation_window_start)
    }else if(estimation_window_start_type == "month"){
      data$estimation_start_date = lubridate::`%m-%`(data$trade_date,
                                                     months(estimation_window_start))
    }else if(estimation_window_start_type == "year"){
      data$estimation_start_date = lubridate::`%m-%`(data$trade_date,
                                                     lubridate::years(estimation_window_start))
    }

    ## 生成估计窗口的终止点
    if(estimation_window_end_type == "day"){
      data$estimation_end_date = data$trade_date - lubridate::days(estimation_window_end)
    }else if(estimation_window_end_type == "month"){
      data$estimation_end_date = lubridate::`%m-%`(data$trade_date,
                                                   months(estimation_window_end))
    }else if(estimation_window_end_type == "year"){
      data$estimation_end_date = lubridate::`%m-%`(data$trade_date,
                                                   lubridate::years(estimation_window_end))
    }

    ## 生成估计窗口中有效交易日的个数
    data$available_trade_days_number =
      sapply(1 : nrow(data), function(i){
        sum((data$trade_date >= data$estimation_start_date[i]) &
              (data$trade_date <= data$estimation_end_date[i]))})


    ## 根据上面的data.frame结构计算超额收益率的函数
    CalculateAR <- function(AR_data, i){

      if(AR_data$available_trade_days_number[i] >= least_valid_days){

        in_window <- (AR_data$trade_date >= AR_data$estimation_start_date[i]) &
          (AR_data$trade_date <= AR_data$estimation_end_date[i])

        R.sub <- AR_data$R[in_window]
        factor1.sub <- data$factor1[in_window]
        factor2.sub <- data$factor2[in_window]
        factor3.sub <- data$factor3[in_window]
        factor4.sub <- data$factor4[in_window]
        factor5.sub <- data$factor5[in_window]

        reg <- lm(R.sub ~ factor1.sub + factor2.sub + factor3.sub + factor4.sub + factor5.sub)
        intercept <- coef(reg)[1]
        coef_factor1 <- coef(reg)[2]
        coef_factor2 <- coef(reg)[3]
        coef_factor3 <- coef(reg)[4]
        coef_factor4 <- coef(reg)[5]
        coef_factor5 <- coef(reg)[6]
        ar <- AR_data$R[i] - (intercept + coef_factor1 * AR_data$factor1[i] +
                                coef_factor2 * AR_data$factor2[i] +
                                coef_factor3 * AR_data$factor3[i] +
                                coef_factor4 * AR_data$factor4[i] +
                                coef_factor5 * AR_data$factor5[i])

        return(ar)
      }else{
        return(NA)
      }
    }

    ## 生成超额收益率ar那一列
    ar = sapply(1 : nrow(data), function(i){CalculateAR(data, i)})


    return(ar)

  }else if(factor_number == 1L){
    ## 将data里面变量的名字改为trade_date, R, factor1
    names(data) <- c("trade_date", "R", "factor1")

    ## 根据estimation_window_start和estimation_window_end生成估计beta系数的估计窗口
    ## 生成估计窗口的起始点
    if(estimation_window_start_type == "day"){
      data$estimation_start_date = data$trade_date - lubridate::days(estimation_window_start)
    }else if(estimation_window_start_type == "month"){
      data$estimation_start_date = lubridate::`%m-%`(data$trade_date,
                                                     months(estimation_window_start))
    }else if(estimation_window_start_type == "year"){
      data$estimation_start_date = lubridate::`%m-%`(data$trade_date,
                                                     lubridate::years(estimation_window_start))
    }

    ## 生成估计窗口的终止点
    if(estimation_window_end_type == "day"){
      data$estimation_end_date = data$trade_date - lubridate::days(estimation_window_end)
    }else if(estimation_window_end_type == "month"){
      data$estimation_end_date = lubridate::`%m-%`(data$trade_date,
                                                   months(estimation_window_end))
    }else if(estimation_window_end_type == "year"){
      data$estimation_end_date = lubridate::`%m-%`(data$trade_date,
                                                   lubridate::years(estimation_window_end))
    }

    ## 生成估计窗口中有效交易日的个数
    data$available_trade_days_number =
      sapply(1 : nrow(data), function(i){
        sum((data$trade_date >= data$estimation_start_date[i]) &
              (data$trade_date <= data$estimation_end_date[i]))})


    ## 根据上面的data.frame结构计算超额收益率的函数
    CalculateAR <- function(AR_data, i){

      if(AR_data$available_trade_days_number[i] >= least_valid_days){

        in_window <- (AR_data$trade_date >= AR_data$estimation_start_date[i]) &
          (AR_data$trade_date <= AR_data$estimation_end_date[i])

        R.sub <- AR_data$R[in_window]
        factor1.sub <- data$factor1[in_window]

        reg <- lm(R.sub ~ factor1.sub)
        intercept <- coef(reg)[1]
        coef_factor1 <- coef(reg)[2]
        ar <- AR_data$R[i] - (intercept + coef_factor1 * AR_data$factor1[i])

        return(ar)
      }else{
        return(NA)
      }
    }

    ## 生成超额收益率ar那一列
    ar = sapply(1 : nrow(data), function(i){CalculateAR(data, i)})


    return(ar)

  }

}







