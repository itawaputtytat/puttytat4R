#' @export
dbGetSrc <- function(name4dbconn, name4src) {

  outputFunProc(R)

  ## Select everything from input-variable = name of table/view
  ## Create string for query
  txt4query <- paste("SELECT * FROM ", name4src, sep = "")

  ## Get data with workaround for UTF-8
  dbWorkaround_UTF8(name4dbconn)
  dat <- dbGetQuery(get(name4dbconn), txt4query, encoding = "UTF-8")

  ## Output
  outputString(paste("* Query data from source:", name4src))

  outputDone()
  return(dat)
}
