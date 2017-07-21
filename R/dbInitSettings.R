#' @title Initialise PostgreSQL settings
#' @description Create tempate for database settings in directory "settings"
#' @export
dbInitSettings<- function(dirname = "dbset", filename = "dbset.R") {
  outputFunProc(R)

  library(RPostgreSQL)

  filepath <- file.path(dirname, paste(filename))
  if (file.exists(filepath)) {
    outputString(paste("* Database settings already exists in", filepath))
    outputString(paste("* Sourcing", filepath))
    source(filepath)
  } else {
    ## Create string for settings
    string4settings <-
      paste(
        "set4db <- c()",
        "set4db$dns <- \"WRITE-DNS-HERE\"",
        "set4db$host <- \"localhost\"",
        "set4db$port <- 5432",
        "set4db$name <- set4db$dns",
        "set4db$user <- \"postgres\"",
        "set4db$pwd  <- \"WRITE-PASSWORD-HERE\"",
        "set4db$drv  <- dbDriver(\"PostgreSQL\")",
        "set4db$select <- c(1, 2)",
        sep = "\n")
    ## Create directy
    dir.create(file.path("settings"), showWarnings = F)
    ## Write template to directory
    writeLines(string4settings, filepath)
    outputString(paste("* Created template for database settings in", filepath))
    outputString("* Please adjust settings")
  }
}
