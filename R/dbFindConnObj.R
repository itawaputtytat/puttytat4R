#' @title  Find object name by dns
#' @description Find object names by dns, if connection was initialised using dbConnectOperator, which adds the original dns as attribute to the object name of the connection.
#' @export
dbFindConnObj <- function(db_name,
                          output = F) {
  outputFunProc(R)

  ## For each object in the environment, which name corresponds to a pattern
  for(i in ls(envir = .GlobalEnv)[grep("db_conn", ls(envir = .GlobalEnv))]) {

    ## Get list of attributes
    attr_list <- attributes(get(i))
    if (attr_list["db_name"] == db_name) {
      result <- i
      break;
    } else {
      result <- NA
    }
  }

  if (is.na(result)) {
    outputString("No connection found")
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
