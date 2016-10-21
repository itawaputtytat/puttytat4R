#' @export

dbQueryRun <- function(dfname4save, string4query) {

  printFunProc(R)

  ## Get data
  dbWorkaround_UTF8()
  dat <- dbGetQuery(dbconn, string4query)

  ## Save data
  assign(dfname4save, dat, envir = .GlobalEnv)
}
