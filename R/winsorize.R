#' Winsorize data
#'
#' \code{winsorize()} winsorizes data to eliminate influence from outliers.
#'
#' @param x A numeric vector.
#' @param method Default to "median". One of "median", "quantile", or "std".
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' winsorize(c(1:10, 100))
winsorize <- function(x, method = "median") {
  if (method == "median") {
    med <- median(x, na.rm = TRUE)
    diff_med <- median(abs(x - med), na.rm = TRUE)
    # 当数据质量太差时，可能出现大部分因子值都是零的情形，取中位数都是零，这种情形下需要把零先忽略掉, 最后把以前是0的部分再恢复成0
    if (diff_med == 0) {
      index_is0 <- x == 0
      x[index_is0] <- NA
      med <- median(x, na.rm = TRUE)
      diff_med <- median(abs(x - med), na.rm = TRUE)
      x[x > med + 5 * diff_med] <- med + 5 * diff_med
      x[x < med - 5 * diff_med] <- med - 5 * diff_med
      x[index_is0] <- 0
    } else {
      x[x > med + 5 * diff_med] <- med + 5 * diff_med
      x[x < med - 5 * diff_med] <- med - 5 * diff_med
    }
  } else if (method == "std") {
    mu <- mean(x, na.rm = TRUE)
    sigma <- sd(x, na.rm = TRUE)
    x[x > mu + 3 * sigma] <- mu + 3 * sigma
    x[x < mu - 3 * sigma] <- mu - 3 * sigma
  } else if (method == "quantile") {
    q <- quantile(x, c(0.01, 0.99), na.rm = TRUE)
    x[x > q[2]] <- q[2]
    x[x < q[1]] <- q[1]
  }

  return(x)
}