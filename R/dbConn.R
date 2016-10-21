#' @title Connect to PostgreSQL database
#' @description Connect to PostgreSQL database
#' @export

dbConn <- function(set4db. = NULL) {

  printFunProc(R)

  ## Check for existing connection
  if (exists("dbconn")) {
    dbDisconnect(dbconn)
    cat("* Disconnected from database \n")
    cat("** Reason: Connection already existed \n")
  }

  ## Connect to database
  if (is.null(set4db.))
    if (exists("set4db"))
      set4db. <- set4db else
        cat("No database settings configured!")

    if (!is.null(set4db.)) {
      dbconn <<-
        dbConnect(set4db.$drv,
                  host     = set4db.$host,
                  port     = set4db.$port,
                  dbname   = set4db.$name,
                  user     = set4db.$user,
                  password = set4db.$pwd)
      cat("* Connected to database")
    }
}
