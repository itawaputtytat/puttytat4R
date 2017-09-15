#' @export
dbConnectOperator <- function(db_sett = sett_db_default,
                              db_name = NULL,
                              db_conn_name = NULL,
                              disconnect_default_db = T) {
  outputFunProc(R)

  ## User selection of requires database
  if (is.null(db_name)) {

    ## Connect to standard database
    catWithSepLine("Fetching list of databases ...")
    pwd <- readline(">>> Enter password: ")
    invisible(
      dbConnectBySettings(db_sett,
                          db_name = db_sett$dns,
                          db_conn_name = db_sett$dns,
                          pwd = pwd)
    )

    ## List available databases
    db_list <-
      dbGetQuery(get(db_sett$dns),
                 "SELECT datname FROM pg_database WHERE datistemplate = FALSE")
    db_list <- unlist(db_list, use.names = F)
    dblist_string <- paste(paste("[", 1:length(db_list), "]", sep = ""),  db_list)

    ## Disconnect default database
    invisible(
      if (disconnect_default_db) {
        objname <- ls(envir = .GlobalEnv, pattern = db_sett$dns)
        eval_string <- paste0("dbDisconnect(", objname, ")")
        eval(parse(text = eval_string))
        if (exists(objname)) {
          eval_string <- paste0("rm(", objname, ", envir = .GlobalEnv)")
          eval(parse(text = eval_string))
        }
      }
    )

    input <- c()
    while(T) {
      catWithSepLine("Select database:")
      outputString(paste(dblist_string, collapse = "\n"))
      input <- readline(">>> ")
      input <- as.numeric(input)
      if (!input %in% 1:length(db_list))
        message("Please enter the list number of the required database") else
          break;
    }
    db_name <- db_list[input]
    catWithSepLine(c("Selected database:", paste0("* ", db_name)))

    if (is.null(db_conn_name))
      db_conn_name <- paste0("db_conn_", input)
  } ## End of user selection

  ## Dynamic naming using numbers
  if (is.null(db_conn_name)) {
    dbconn_nr <- length(grep("db_conn_", ls(envir = .GlobalEnv)))
    db_conn_name <- paste0("db_conn_", dbconn_nr + 1)
  }


  ## Password
  if(length(db_sett$pwd) == 0)
    pwd <- readline(">>> Enter password: ") else
      pwd <- db_sett$pwd

    ## Connect to target database
    dbConnectBySettings(db_sett,
                        db_name = db_name,
                        db_conn_name = db_conn_name,
                        pwd = pwd)

    ## Add db_name to connection attributes
    eval_string <-
      paste0("attr(", db_conn_name, ", \"db_name\") <- \"", db_name, "\"")
    eval(parse(text = eval_string), envir = .GlobalEnv)
}
