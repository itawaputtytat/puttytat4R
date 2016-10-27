#' @title Remove views or tables from database
#' @param name4src. character. Name for view or table
#' @export
dbRemoveSrc <- function(name4src, dbconn) {
  outputFunProc(R)

  ## Check if source is table or view
  if (name4src %in% dbListViews(dbconn) )
    srctype = "VIEW" else
      srctype = "TABLE"

  ## Select everything from input variable = name of table/view
  ## Create string for query and send query
  dbquery$string <- paste("DROP", srctype, "IF EXISTS", name4src, "CASCADE")
  dat <- dbSendQuery(dbconn, dbquery$string)

  ## Output
  txt4output <- paste("* Removed:", srctype, "with name:", name4src)
  outputString(txt4output)
  outputDone()
}
