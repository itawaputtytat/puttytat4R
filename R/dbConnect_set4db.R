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

  name4dbconn <- paste("dbconn", name4dbconn, sep = "_")

  if (!is.null(settings)) {
    assign(name4dbconn,
           dbConnect(set4db$drv,
                     host     = set4db$host,
                     port     = set4db$port,
                     dbname   = set4db$name,
                     user     = set4db$user,
                     password = set4db$pwd),
           envir = .GlobalEnv)

    outputString(paste("* Connected to database:", set4db$name))
    outputString(paste("** See object:", name4dbconn))
  }
}
