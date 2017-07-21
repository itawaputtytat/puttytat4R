#' @title Connect to PostgreSQL database
#' @description Connect to PostgreSQL database
#' @export

dbConnect_set4db <- function(settings, name4dbconn = "dbconn") {

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

  name4dbconn <- paste0("dbconn_", name4dbconn)

  if (!is.null(settings)) {
    assign(name4dbconn,
           dbConnect(settings$drv,
                     host     = settings$host,
                     port     = settings$port,
                     dbname   = settings$name,
                     user     = settings$user,
                     password = settings$pwd),
           envir = .GlobalEnv)

    outputString(paste("* Connected to database:", settings$name))
    outputString(paste("** See object:", name4dbconn))
  }
}
