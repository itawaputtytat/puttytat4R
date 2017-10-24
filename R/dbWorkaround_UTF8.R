#' @export

dbWorkaround_UTF8 <- function(db_conn_name) {
  outputFunProc(R)
  postgresqlpqExec(get(db_conn_name), "SET client_encoding = 'windows-1252'")
}

