#' Winsorize data
#'
#' \code{winsorize()} winsorizes data to eliminate influence from outliers.
#'
#' @param x A numeric vector.
#' @param method Default to "quantile". One of "quantile", "median", or "std".
#' Or a numeric vector of length 2 specify lower and upper bounds.
#' @param side Side to winsorize. Default to "both". "both", "right", or "left".
#' @param fill Default "bound" to assign outliers to lower and (or) upper bounds, or "NA
#' to assign outliers to be NA.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' winsorize(c(1:10, 100))
winsorize <- function(x, method = "quantile", side = "both", fill = "bound") {
  if (is.numeric(method) & length(method) == 2L) {
    upper_bound <- method[2]
    lower_bound <- method[1]
  } else if (method == "quantile") {
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

  if (fill == "NA") {
    if (side == "right") {
      x[x > upper_bound] <- NA
    } else if (side == "left") {
      x[x < lower_bound] <- NA
    } else {
      x[x > upper_bound] <- NA
      x[x < lower_bound] <- NA
    }
  } else {
    if (side == "right") {
      x[x > upper_bound] <- upper_bound
    } else if (side == "left") {
      x[x < lower_bound] <- lower_bound
    } else {
      x[x > upper_bound] <- upper_bound
      x[x < lower_bound] <- lower_bound
    }
  }

  return(x)
}
