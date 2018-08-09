#' Winsorize data
#'
#' \code{winsorize()} winsorizes data to eliminate influence from outliers.
#'
#' @param x A numeric vector.
#' @param method Default to "quantile". One of "quantile", "median", or "std".
#' @param side Side to winsorize. Default to "both". "both", "right", or "left".
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' winsorize(c(1:10, 100))
winsorize <- function(x, method = "quantile", side = "both") {
  if (method == "quantile") {
    upper_bound <- quantile(x, 0.99, na.rm = TRUE)
    lower_bound <- quantile(x, 0.01, na.rm = TRUE)
  } else if (method == "median") {
    med <- median(x, na.rm = TRUE)
    diff_med <- median(abs(x - med), na.rm = TRUE)
    upper_bound <- med + 5 * diff_med
    lower_bound <- med - 5 * diff_med
  } else if (method == "std") {
    mu <- mean(x, na.rm = TRUE)
    sigma <- sd(x, na.rm = TRUE)
    upper_bound <- mu + 3 * sigma
    lower_bound <- mu - 3 * sigma
  } else {
    stop("Invalid method")
  }

  if (side == "right") {
    x[x > upper_bound] <- upper_bound
  } else if (side == "left") {
    x[x < lower_bound] <- lower_bound
  } else {
    x[x > upper_bound] <- upper_bound
    x[x < lower_bound] <- lower_bound
  }

  return(x)
}
