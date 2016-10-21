#' @export

dbWorkaround_UTF8 <- function() {
  printFunProc(R)
  postgresqlpqExec(dbconn, "SET client_encoding = 'windows-1252'")
}

