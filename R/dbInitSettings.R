#' @title Initialise PostgreSQL settings
#' @description Create tempate for database settings in directory "settings"
#' @export
dbInitSettings <- function(dir_name = "settings",
                           file_name = "sett_db_default.R",
                           dns_default = "\"postgres\"",
                           set_to_default = F) {
  outputFunProc(R)

  library(RPostgreSQL)

  ## Check if settings already exist and should be reset
  input <- "n"
  file_path <- file.path(dir_name, paste(file_name))
  if (file.exists(file_path) & set_to_default) {

    outputString("Database settings already exist in:")
    outputString(paste("*", file_path))

    while(T) {
      input <- readline(">>> Reset settings to defaults? [y/n]: ")
      if (tolower(input) != "y" | tolower(input) != "n") {
        break
      }
    }
  }

  if (!file.exists(file_path) | tolower(input) == "y") {

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

  } else {

    outputString("Loading database settings from:")
    outputString(paste("*", file_name))
    source(file_path)

  }
}
