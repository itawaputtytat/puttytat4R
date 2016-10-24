#' @export

codeOutliersZ <- function(dat,
                          zCutOff = 1.96,
                          replace = NA,
                          returnZValues = FALSE,
                          digits = 2) {

  outputFunProc(R)

  ## Compute SD (sample version n = n [not n-1])
  stdev <-
    sqrt(sum( (dat - mean(dat, na.rm = T))^2, na.rm = T ) /
           sum(!is.na(dat)))

  ## Compute absolute z values for each values
  absZ <- abs(dat - mean(dat, na.rm = T)) / stdev

  ## Subset data that has absZ greater than the zCutOff
  ## ...and replace them with replace
  ## Can also replace with other values (such as max/mean of data)
  dat[absZ > zCutOff] <- replace

  ## If values == TRUE, return z score for each value
  if (returnZValues) {
    #return(round(absZ, digits))
    return(absZ) #if values == TRUE, return z score for each value

    ## Otherwise, return values with outliers replaced
  } else {
    #return(round(dat, digits))
    return(dat)
  }

}
