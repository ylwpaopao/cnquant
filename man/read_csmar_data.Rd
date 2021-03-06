% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_csmar_data.R
\name{read_csmar_data}
\alias{read_csmar_data}
\title{Read CSMAR data}
\usage{
read_csmar_data(path, skip = 0, col_names = TRUE, col_types = NULL)
}
\arguments{
\item{path}{Path to a raw data file.}

\item{skip}{Integer. The number of lines of the data file to skip before beginning to read data.
Usually 1 for 'Excel Format Create R Data' from CSMAR.}

\item{col_names}{"Chinese" to use chinese column names from the external txt description file.
Otherwise the same with \code{read_csv} and \code{read_excel}.}

\item{col_types}{The same with \code{read_csv} and \code{read_excel}.}
}
\value{
A tibble.
}
\description{
\code{read_csmar_data()} reads a data file exported from CSMAR into R. A
wrapper of \code{read_csv} and \code{read_excel}.
}
\seealso{
\code{\link[readr]{read_csv}} and \code{\link[readxl]{read_excel}}.
}
