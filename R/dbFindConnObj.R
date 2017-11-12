#' @title  Find object name by dns
#' @description Find object names by dns, if connection was initialised using dbConnectOperator, which adds the original dns as attribute to the object name of the connection.
#' @export
dbFindConnObj <- function(db_name,
                          output = F,
                          ...) {
  outputFunProc(R)

  names_finder <- grep("db_conn", ls(envir = .GlobalEnv))
  names_db_conn <- ls(envir = .GlobalEnv)[names_finder]

  result <- c()
  if (!length(names_db_conn) == 0) {

    ## For each object in the environment, which name corresponds to a pattern
    for(i in names_db_conn) {

      ## Get list of attributes
      attr_list <- attributes(get(i))
      if (length(attr_list) == 0) {
        next
      } else {
        if (grepl(db_name, attr_list["db_name"])) {
          result <- c(result, i)
        }
      }
    }
  }


  if (length(result) > 1) {
    outputString("Multiple objects found:")
    outputString(paste("*", result))
    outputString("** Only the first input will be returned")
    return(result[1])
  }


  if (length(result) == 0) {

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
    if (length(result) != 0) {
      outputString(c("Found db_name attribute: ",
                     paste0("\"", db_name, "\""),
                     "in:"),
                   output_sepline = T, sepline_pos = "b")
      outputString(c(result, "\n"))
    }
  }

  return(result)
}
