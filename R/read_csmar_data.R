#' Read CSMAR data
#'
#' \code{read_csmar_data()} reads a data file exported from CSMAR into R. A
#' wrapper of \code{read_csv} and \code{read_excel}.
#'
#' @param path Path to a raw data file.
#' @param skip Integer. The number of lines of the data file to skip before beginning to read data.
#'   Usually 1 for 'Excel Format Create R Data' from CSMAR.
#' @param col_names "Chinese" to use chinese column names from the external txt description file.
#'   Otherwise the same with \code{read_csv} and \code{read_excel}.
#' @param col_types The same with \code{read_csv} and \code{read_excel}.
#'
#' @return A tibble.
#' @export
#'
#' @seealso \code{\link[readr]{read_csv}} and \code{\link[readxl]{read_excel}}.
#'
#' @examples
read_csmar_data <- function(path, skip = 1, col_names = TRUE, col_types = NULL) {
  ### import the first file of data, return the whole combined dataset, in the case of a seqence of ".xls" data
  ### otherwise, directly import the single data file (for ".csv" and ".txt")

  stopifnot(file.exists(path))

  if (col_names == "Chinese") {
    col_names <- path %>%
      dirname() %>%
      dir(pattern = "\\[DES\\]") %>%
      `[`(length(.)) %>%
      file.path(dirname(path), .) %>%
      readLines(encoding = "UTF-8") %>%
      sub("^.+\\[(.+)\\].+$", "\\1", .)
  }

  if (grepl(".xls$", path)) {
    if (is.character(col_types) & col_names == "Chinese") col_names <- col_names[col_types != "skip"]
    return(path %>%
             dirname() %>%
             dir(pattern = paste0("^", unlist(strsplit(basename(path), "\\."))[1], "-?", "[0-9]*\\.xls$")) %>%
             file.path(dirname(path), .) %>%
             lapply(readxl::read_excel, skip = skip, col_names = col_names, col_types = col_types) %>%
             bind_rows())
  } else if (grepl(".csv$", path)) {
    if (is.character(col_types)) col_names <- col_names[unlist(strsplit(col_types, "")) != "_"]
    return(readr::read_csv(path, skip = skip, col_names = col_names, col_types = col_types))
  } else if (grepl(".txt$", path)) {
    if (is.character(col_types)) col_names <- col_names[unlist(strsplit(col_types, "")) != "_"]
    return(readr::read_tsv(path, skip = skip, col_names = col_names, col_types = col_types))
  } else {
    stop("unknown file type")
  }
}
