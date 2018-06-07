devide <- function(x, n_group = 10) {
  x <- dplyr::row_number(x)
  return(cut(
    x,
    breaks = quantile(x,
                      probs = seq(0, 1, length.out = n_group + 1),
                      na.rm = TRUE),
    include.lowest = TRUE,
    labels = FALSE
  ))
}
