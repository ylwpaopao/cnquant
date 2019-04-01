#' Show the number of NAs of each column of a data frame
#'
#' @param .data A data frame.
#'
#' @return
#' @export
#'
#' @examples
show_na <- function(.data) {
  .data %>%
    sapply(function(x) sum(is.na(x))) %>%
    return()
}
