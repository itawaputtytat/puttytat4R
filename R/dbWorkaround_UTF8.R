#' @export

dbWorkaround_UTF8 <- function(name4dbconn) {
  outputFunProc(R)
  postgresqlpqExec(get(name4dbconn), "SET client_encoding = 'windows-1252'")
}

