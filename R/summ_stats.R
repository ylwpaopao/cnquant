#' Calculate summary statistics
#'
#' \code{summ_stats()} calculates frequently used summary statistics for academic paper.
#'
#' @param .data
#' @param round
#' @param lang
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
summ_stats <- function(.data, round = NULL, lang = "ENG", ...) {
  UseMethod("summ_stats")
}

#' Title
#'
#' @param .data
#' @param round
#' @param lang
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
summ_stats.default <- function(.data, round = NULL, lang = "ENG", ...) {
  .data <- .data[!is.na(.data)]

  Summ_Stats <- c(
    "Mean" = mean(.data),
    "Median" = median(.data),
    "Std.Dev." = sd(.data),
    "Min" = min(.data),
    "Q" = quantile(.data, 0.1),
    "Q" = quantile(.data, 0.9),
    "Max" = max(.data),
    "N" = length(.data)
  )

  if (!is.null(round)) {
    Summ_Stats <- round(Summ_Stats, round)
  }

  if (lang == "CHN") {
    names(Summ_Stats) <- c(
      "均值",
      "中位数",
      "标准差",
      "最小值",
      "10%分位数",
      "90%分位数",
      "最大值",
      "样本数量"
    )
  }

  return(Summ_Stats)
}

#' Title
#'
#' @param .data
#' @param round
#' @param lang
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
summ_stats.data.frame <- function(.data, round = NULL, lang = "ENG", ...) {
  Summ_Stats <- .data %>%
    dplyr::select_if(~ is.numeric(.) | is.logical(.)) %>%
    sapply(summ_stats, round = round, lang = lang) %>%
    t()

  return(Summ_Stats)
}
