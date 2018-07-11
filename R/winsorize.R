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
  }
  return(x)
}
