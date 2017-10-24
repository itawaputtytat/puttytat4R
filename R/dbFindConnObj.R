#' @title  Find object name by dns
#' @description Find object names by dns, if connection was initialised using dbConnectOperator, which adds the original dns as attribute to the object name of the connection.
#' @export
dbFindConnObj <- function(db_name,
                          output = F,
                          ...) {
  outputFunProc(R)

  names_finder <- grep("db_conn", ls(envir = .GlobalEnv))
  names_db_conn <- ls(envir = .GlobalEnv)[names_finder]

  result <- NA
  if (!length(names_db_conn) == 0) {

    ## For each object in the environment, which name corresponds to a pattern
    for(i in names_db_conn) {
      ## Get list of attributes
      attr_list <- attributes(get(i))
      if (attr_list["db_name"] == db_name) {
        result <- i
      }
    }
  }

  if (is.na(result)) {

    outputString(paste("No connection object found for:", db_name))
    outputString("Search in list of available databases?")
    input <- readline("[y/n] >>> ")
    if (input == "y") {
      dbConnectOperator(db_name = db_name,
                        return_db_conn_name = T,
                        ...)
      result <- dbFindConnObj(db_name = db_name, output = output, ...)
    }
  }

  if (output) {
    if (!is.na(result)) {
      outputString(c("Found db_name attribute: ",
                     paste0("\"", db_name, "\""),
                     "in:"),
                   output_sepline = T, sepline_pos = "b")
      outputString(c(result, "\n"))
    }
  }
  return(result)
}
