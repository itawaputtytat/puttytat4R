#' @title Connect to PostgreSQL database
#' @description Connect to PostgreSQL database
#' @export

dbConnectBySettings <- function(settings,
                                dbname = settings$dns,
                                dbconn_name = settings$dns,
                                pwd) {

  outputFunProc(R)

  ## Check for existing connection
  if (exists(dbconn_name)) {
    dbDisconnect(get(dbconn_name))
    catWithSepLine(c("Disconnected database:",
                     paste0("* ", dbconn_name),
                     "Reason: Connection already existed"))
  }

  ## Connect to database
  if (is.null(settings))
    cat("No database settings configured!")

  if (!is.null(settings)) {
    assign(dbconn_name,
           dbConnect(settings$drv,
                     host     = settings$host,
                     port     = settings$port,
                     dbname   = dbname,
                     user     = settings$user,
                     password = pwd),
           envir = .GlobalEnv)

    catWithSepLine(c("Connected database:",
                     paste0("* ", dbname),
                     "Refer to object:",
                     paste0("* ", dbconn_name)))
  }
}
