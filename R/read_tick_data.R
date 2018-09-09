#' Read tick data
#'
#' \code{read_tick_data()} reads a certain format csv data as is high frequency
#' stock tick data exported from Wind. A wrapper of \code{read_csv}.
#'
#' @param file Path to a file.
#' @param col_types There are two forms.
#'
#' The first is same as \code{read_csv}.
#' The second is a character vector which contains variable names you want to use.
#'
#' @return A tibble.
#' @export
#'
#' @seealso \code{\link[readr]{read_csv}}.
#'
#' @examples
read_tick_data <- function(file,
                           col_types = readr::cols_only(
                             time = readr::col_integer(),
                             price = readr::col_integer(),
                             volume = readr::col_integer(),
                             turover = readr::col_integer(),
                             match_items = readr::col_integer(),
                             bs_flag = readr::col_integer(),
                             accvolume = readr::col_integer(),
                             accturover = readr::col_integer(),
                             ask10 = readr::col_integer(),
                             ask9 = readr::col_integer(),
                             ask8 = readr::col_integer(),
                             ask7 = readr::col_integer(),
                             ask6 = readr::col_integer(),
                             ask5 = readr::col_integer(),
                             ask4 = readr::col_integer(),
                             ask3 = readr::col_integer(),
                             ask2 = readr::col_integer(),
                             ask1 = readr::col_integer(),
                             bid1 = readr::col_integer(),
                             bid2 = readr::col_integer(),
                             bid3 = readr::col_integer(),
                             bid4 = readr::col_integer(),
                             bid5 = readr::col_integer(),
                             bid6 = readr::col_integer(),
                             bid7 = readr::col_integer(),
                             bid8 = readr::col_integer(),
                             bid9 = readr::col_integer(),
                             bid10 = readr::col_integer(),
                             asize10 = readr::col_integer(),
                             asize9 = readr::col_integer(),
                             asize8 = readr::col_integer(),
                             asize7 = readr::col_integer(),
                             asize6 = readr::col_integer(),
                             asize5 = readr::col_integer(),
                             asize4 = readr::col_integer(),
                             asize3 = readr::col_integer(),
                             asize2 = readr::col_integer(),
                             asize1 = readr::col_integer(),
                             bsize1 = readr::col_integer(),
                             bsize2 = readr::col_integer(),
                             bsize3 = readr::col_integer(),
                             bsize4 = readr::col_integer(),
                             bsize5 = readr::col_integer(),
                             bsize6 = readr::col_integer(),
                             bsize7 = readr::col_integer(),
                             bsize8 = readr::col_integer(),
                             bsize9 = readr::col_integer(),
                             bsize10 = readr::col_integer(),
                             ask_av_price = readr::col_integer(),
                             bid_av_price = readr::col_integer(),
                             total_ask_volume = readr::col_integer(),
                             total_bid_volume = readr::col_integer()
                           )) {
  if(is.list(col_types)){
    purrr::safely(readr::read_csv)(file, col_types = col_types)$result %>%
      return()
  }else if(is.character(col_types)){
    data <- readr::read_csv(file)

    data_variable_names <- names(data)

    if (all(col_types %in% data_variable_names)) {
      data <- data %>%
        dplyr::select(col_types)

      return(data)
    }
  }

}
