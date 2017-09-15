#' @title Connect to PostgreSQL database
#' @description Connect to PostgreSQL database
#' @export

dbConnectBySettings <- function(sett = sett_db_default,
                                db_name = sett$dns,
                                db_conn_name = sett$dns,
                                pwd) {

  outputFunProc(R)

  ## Check for existing connection
  if (length(db_conn_name) == 0) {
    dbDisconnect(get(db_conn_name))
    catWithSepLine(c("Disconnected database:",
                     paste0("* ", db_conn_name),
                     "Reason: Connection already existed"))
  }

  ## Connect to database
  if (is.null(sett))
    cat("No database settings configured!")

  if (!is.null(sett)) {
    assign(db_conn_name,
           dbConnect(sett$drv,
                     host     = sett$host,
                     port     = sett$port,
                     dbname   = db_name,
                     user     = sett$user,
                     password = pwd),
           envir = .GlobalEnv)

    catWithSepLine(c("Connected database:",
                     paste0("* ", db_name),
                     "Refer to object:",
                     paste0("* ", db_conn_name)))
  }
}
