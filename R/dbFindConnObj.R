#' @export
dbFindConnObj <- function(db_name, output = F) {
  outputFunProc(R)
  for(i in ls(envir = .GlobalEnv)[grep("db_conn", ls(envir = .GlobalEnv))]) {
    attr_list <- attributes(get(i))
    while(T) {
      if (attr_list["db_name"] == db_name)
        break;
    }
  }
  if (output) {
    outputString(c("Found db_name attribute: ",
                   paste0("\"", db_name, "\""),
                   "in:"),
                 output_sepline = T, sepline_pos = "b")
    outputString(c(i, "\n"))
  }
  return(i)
}
