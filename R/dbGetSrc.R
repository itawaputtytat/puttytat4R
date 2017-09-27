#' @title Query complete database source
#' @description Sources my be tables or views
#' @param dbconn A DBIConnection object, as produced by dbConnect or character of the object name
#' @param name4src Name of database source as character
#' @export
dbGetSrc <- function(dbconn, name4src) {

  outputFunProc(R)

  ## Select everything from input-variable = name of table/view
  ## Create string for query
  txt4query <- paste("SELECT * FROM ", "\"", name4src, "\"", sep = "")

  ## Get data with workaround for UTF-8
  if(!is.character(dbconn))
    dbconn <- eval(deparse(substitute(dbconn)))
  dbWorkaround_UTF8(dbconn)
  dat <- dbGetQuery(get(dbconn), txt4query, encoding = "UTF-8")

  ## Output
  outputString(paste("* Queried data from source:", name4src))

  outputDone()
  return(dat)
}
