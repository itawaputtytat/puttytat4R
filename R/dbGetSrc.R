#' @export

dbGetSrc <- function(name4src) {

  printFunProc(R)

  ## Select everything from input-variable = name of table/view
  ## Create string for query
  txt4query <- paste("SELECT * FROM ", name4src, sep = "")

  ## Get data with workaround for UTF-8
  dbWorkaround_UTF8()
  dat <- dbGetQuery(dbconn, txt4query, encoding = "UTF-8")

  ## Output
  cat("* Query data from source: ", name4src, "\n", sep = "")
  printDone()

  return(dat)
}