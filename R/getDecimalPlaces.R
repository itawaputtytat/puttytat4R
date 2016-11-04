#' @title Get number of decimal places
#' @description Necessary for correct value presentation after interpolation using spline
#' @export


getDecimalPlaces <- function(values,
                             output = puttytat4R_env$outputFunProc_status) {

  outputFunProc(R)

  ## Disable scientific notation
  options_backup <- options()
  options("scipen" = 100)

  ## Remove NA values
  values <- values[which(!is.na(values))]

  decplaces <- sapply(values, function(x) {
    ## Only if there is a rest when dividing
    if ((x %% 1) != 0) {
      ## ... compute number of decimal places
      ## ... by converting x to a string
      ## ... and count number of character behing decimal indicator "."
      x <- as.character(x)
      decplaces <- strsplit(sub('0+$', '', x), ".", fixed = TRUE)[[1]][[2]]
      decplaces <- nchar(decplaces)
      ## Otherwise
    } else {
      ## Otherwise
      decplaces <- 0
    }
    return(decplaces)
  } )
  decplaces <- max(decplaces)

  ## Restore options
  options(options_backup)

  if (output) {
    cat("* Number of decimal places: ", decplaces, "\n", sep = "")
    outputDone()
  }
  return(decplaces)
}
