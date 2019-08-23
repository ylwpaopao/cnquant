#' Transform from data.frame to time series
#'
#' The data.frame should have one column as index and others as time series.
#'
#' @param df
#' @param .datetime index.
#' @param output_class zoo or ts.
#' @param frequency 4L or 12L (or NULL) for ts (zoo).
#'
#' @return
#' @export
#'
#' @examples
df_to_ts <- function(df, .datetime = "DATETIME", output_class = "ts", frequency = 12L) {

  if (output_class == "zoo") {

    df %>%
      column_to_rownames(.datetime) %>%
      zoo::as.zoo(order.by = ymd(rownames(.)), frequency = frequency) %>%
      return()

  } else if (output_class == "ts") {

    if (frequency == 4L) {
      period_start <- df %>% pull(.datetime) %>% first() %>% {c(year(.), quarter(.))}
      period_end <- df %>% pull(.datetime) %>% last() %>% {c(year(.), quarter(.))}
    } else if (frequency == 12L) {
      period_start <- df %>% pull(.datetime) %>% first() %>% {c(year(.), month(.))}
      period_end <- df %>% pull(.datetime) %>% last() %>% {c(year(.), month(.))}
    } else
      stop("invalid frequency")

    df %>%
      column_to_rownames(.datetime) %>%
      ts(start = period_start, end = period_end, frequency = frequency) %>%
      return()

  } else

    stop("unsupported output class, currently only zoo & ts are supported")

}

#' Title
#'
#' @param y
#'
#' @return
#' @export
#'
#' @examples
forecast_arima_h1 <- function(y) {
  # 在rollapply时y是一个zoo
  y %>%
    # 这里auto.arima会将其变为一个ts
    forecast::auto.arima() %>%
    forecast::forecast(h = 1L) %>%
    .$mean %>%
    as.numeric() %>%
    return()
}

#' Title
#'
#' @param ts
#'
#' @return
#' @export
#'
#' @examples
ts_to_df <- function(ts) {
  bind_cols(
    DATETIME = ts %>% as.zoo() %>% index() %>% as.Date() %>% month_end(),
    as_tibble(ts)
  ) %>%
    return()
}
