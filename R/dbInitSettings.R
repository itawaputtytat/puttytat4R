#' @title Initialise PostgreSQL settings
#' @description Create tempate for database settings in directory "settings"
#' @export
dbInitSettings <- function(dir_name = "sett_db_default_default",
                          filename = "sett_db_default_default.R",
                          dns_default = "\"postgres\"") {
  outputFunProc(R)

  library(RPostgreSQL)

  file_path <- file.path(dir_name, paste(filename))
  if (file.exists(file_path)) {
    outputString(paste("* Database settings already exists in", file_path))
    outputString(paste("* Sourcing", file_path))
    source(file_path)
  } else {
    ## Create string for settings
    settings_string <-
      paste(
        "sett_db_default <- c()",
        "sett_db_default$dns <- ", dns_default,
        "sett_db_default$host <- \"localhost\"",
        "sett_db_default$port <- 5432",
        "sett_db_default$user <- \"postgres\"",
        "sett_db_default$drv  <- dbDriver(\"PostgreSQL\")",
        sep = "\n")
    ## Create directory
    dir.create(file.path(dir_name), showWarnings = F, recursive = T)
    ## Write template to directory
    writeLines(settings_string, file_path)
    outputString(paste("* Created template for database settings in", file_path))
    outputString("* Please adjust settings")
  }
}
