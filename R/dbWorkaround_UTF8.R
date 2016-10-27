#' @export

dbWorkaround_UTF8 <- function(dbconn) {
  outputFunProc(R)
  postgresqlpqExec(dbconn, "SET client_encoding = 'windows-1252'")
}

