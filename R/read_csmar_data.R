#' Title
#'
#' @param path
#' @param skip
#' @param col_names
#' @param col_types
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr %>%

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
