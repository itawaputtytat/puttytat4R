#' @title Query complete database source
#' @description Sources my be tables or views
#' @param db_conn_name A DBIConnection object, as produced by db_conn_nameect or character of the object name
#' @param db_src_name Name of database source as character
#' @export
dbGetSrc <- function(db_conn_name,
                     db_src_name,
                     add_col_db_src_name = F,
                     arrange_by_col_name = NULL) {

  outputFunProc(R)

  ## Select everything from input-variable = name of table/view
  ## Create string for query
  query_string <- paste("SELECT * FROM ", "\"", db_src_name, "\"", sep = "")

  ## Get data with workaround for UTF-8
  if (!is.character(db_conn_name)) {
    db_conn_name <- deparse(substitute(db_conn_name))
  }

  dbWorkaround_UTF8(db_conn_name)
  dat <- dbGetQuery(get(db_conn_name), query_string, encoding = "UTF-8")

  if (!is.null(arrange_by_col_name)) {
    dat <-
      dat %>%
      arrange_(arrange_by_col_name)
  }

  ## Set attribute: Source name
  eval_string <-
    paste0("attr(dat, \"db_src_name\") <- \"", db_src_name, "\"")
  eval(parse(text = eval_string))

  ## Output
  outputString(paste("* Queried data from source:", db_src_name))

  if (add_col_db_src_name) {
    dat$db_src_name <- db_src_name
  }

  return(dat)
}
