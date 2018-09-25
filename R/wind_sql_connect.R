#' Title
#'
#' @return
#' @export
#'
#' @examples
wind_sql_connect <- function() {
  # under configuration files odbc.ini & odbc.yaml
  conn <- DBI::dbConnect(odbc::odbc(),
                         DSN = "TDS",
                         UID = yaml::yaml.load_file("/etc/odbc.yaml")$TDS$UID,
                         PWD = yaml::yaml.load_file("/etc/odbc.yaml")$TDS$PWD
  )
  return(conn)
}
