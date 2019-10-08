#' Generate an index dictionary
#'
#' Read in an excel file, and generate an index dictionary as a named chr.
#' The excel spreadsheet should have 2 columns named code & name respectively.
#'
#' @param path
#' @param sheet
#'
#' @return
#' @export
#'
#' @examples
gen_index_dict <- function(path, sheet = NULL) {
  index_dict <- readxl::read_excel(path, sheet = sheet)
  code <- index_dict$code
  index_dict <- index_dict$name
  names(index_dict) <- code
  return(index_dict)
}

#' Revised w.edb for friendly Chinese index names
#'
#' Read in an excel file including names and codes, and get relative data from Wind terminal.
#' The excel spreadsheet should have 2 columns named code & name respectively.
#'
#' @param path
#' @param sheet
#' @param beginTime
#' @param endTime
#'
#' @return
#' @export
#'
#' @examples
w_edb <- function(path, sheet = NULL, beginTime = getOption("beginTime", "20010101"), endTime = getOption("endTime", Sys.Date())) {
  index_dict <- gen_index_dict(path, sheet = sheet)
  codes <- names(index_dict)
  Data <- w.edb(codes, beginTime = beginTime, endTime = endTime) %>%
    .$Data %>%
    as_tibble() %>%
    # 将变量名由code转化为中文名称
    `names<-`(c("DATETIME", index_dict))
  return(Data)
}
