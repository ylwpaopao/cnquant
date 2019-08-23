#' Estimate component weight
#'
#' @param df The first column should be the overall value, and the remained are the components.
#'
#' @return
#' @export
#'
#' @examples
weight_estimate <- function(df) {

  # number of explanary varibles
  n_dim <- ncol(df) - 1L

  df <- df %>% filter(complete.cases(.))

  # cov matrix (square sum matrix)
  cov_matrix <- df %>%
    cov.wt(center = FALSE, method = "ML") %>%
    .$cov
  cov_matrix <- cov_matrix * nrow(df)

  Dmat <- cov_matrix[-1, -1]
  dvec <- cov_matrix[-1, 1]

  identity_matrix <- diag(n_dim)
  seq_2n <- matrix(1:(2 * n_dim), nrow = 2L, byrow = TRUE) %>%
    as.vector()
  Amat <- cbind(identity_matrix, -identity_matrix) %>%
    .[, seq_2n] %>%
    cbind(rep(1, n_dim), rep(-1, n_dim))

  bvec <- c(0, -1) %>%
    rep(n_dim) %>%
    c(1, -1)

  # solve
  k <- solve.QP(Dmat, dvec, Amat, bvec)

  # output
  RMSE <- sqrt((k$value * 2 + cov_matrix[1, 1]) / nrow(df))
  print(paste("RMSE:", RMSE %>% scales::percent()))
  return(k$solution)

}
