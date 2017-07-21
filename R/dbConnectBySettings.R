#' @title Connect to PostgreSQL database
#' @description Connect to PostgreSQL database
#' @export

dbConnectBySettings <- function(settings, dbconn_name = "dbconn") {

  outputFunProc(R)

  dbconn_name <- paste0("dbconn_", dbconn_name)

  ## Check for existing connection
  if (exists(dbconn_name)) {
    dbDisconnect(get(dbconn_name))
    cat("Disconnected from database \n")
    cat("* Reason: Connection already existed \n")
  }

  ## Connect to database
  if (is.null(settings))
    cat("No database settings configured!")

  if (!is.null(settings)) {
    assign(dbconn_name,
           dbConnect(settings$drv,
                     host     = settings$host,
                     port     = settings$port,
                     dbname   = settings$name,
                     user     = settings$user,
                     password = settings$pwd),
           envir = .GlobalEnv)

    outputString(paste("* Connected to database:", settings$name))
    outputString(paste("** Refer to object:", dbconn_name))
  }
}
