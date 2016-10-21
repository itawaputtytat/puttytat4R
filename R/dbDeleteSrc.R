#' @export

dbDeleteSrc <- function(name4src) {

  printFunProc(R)

  ## Check if source is table or view
  dbviews <- dbListViews(dbconn)
  if (name4src %in% dbviews)
    srctype = "VIEW" else
      srctype = "TABLE"

  ## Select everything from input-variable = name of table/view
  ## Create string for query
  string4query <- paste("DROP", srctype, "IF EXISTS", name4src, "CASCADE")

  ## Send query
  dbSendQuery(dbconn, string4query)

  ## Output
  cat("* Deleted source type:", srctype, "with name:", name4src, "\n")
  printDone()
}
