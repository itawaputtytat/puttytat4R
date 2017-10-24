#' @export
dbDisconnectAll <- function(drv = sett_db_default$drv) {
  ## Get all connections and count
  db_connections <- dbListConnections(sett_db_default$drv)
  db_connections_n <- length(db_connections)
  ## Disconnect all connections
  invisible(lapply(db_connections, dbDisconnect))
  outputWithSepLine(paste("* Closed", db_connections_n, "connections"))
}
