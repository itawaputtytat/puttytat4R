#' @export

dbListViews <- function(dbconn, ...) {

  outputFunProc(R)

  out <- dbGetQuery(dbconn, "SELECT viewname
                             FROM pg_views
                             WHERE schemaname !='information_schema' AND
                                   schemaname !='pg_catalog'", ...)
  if (is.null(out) || nrow(out) == 0) out<-character(0)
  else out <- out[, 1]
  return(out)
}
