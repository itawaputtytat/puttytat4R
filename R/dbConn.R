#' @title Connect to PostgreSQL database
#' @description Connect to PostgreSQL database
#' @export

dbConn <- function(settings, name4dbconn = "dbconn") {

  outputFunProc(R)

  ## Check for existing connection
  if (exists("dbconn")) {
    dbDisconnect(dbconn)
    cat("Disconnected from database \n")
    cat("* Reason: Connection already existed \n")
  }

  ## Connect to database
  if (is.null(settings))
    cat("No database settings configured!")

  name4dbconn <- paste("dbconn", name4dbconn, sep = "_")

  if (!is.null(settings)) {
    assign(name4dbconn,
           dbConnect(settings$drv,
                     host     = settings$host,
                     port     = settings$port,
                     dbname   = settings$name,
                     user     = settings$user,
                     password = settings$pwd),
           envir = .GlobalEnv)

    cat("Connected to database:", "\n")
    cat(">", settings$name)
  }
}
