#' @title Flag outlier above z = 1.96
#' @export

codeOutliersZ <- function(dat,
                          zCutOff = 1.96,
                          returnZValues = FALSE) {

  outputFunProc(R)

  ## Compute SD (sample version n = n [not n-1])
  stdev <-
    sqrt(sum( (dat - mean(dat, na.rm = T))^2, na.rm = T ) /
           sum(!is.na(dat)))

  ## Compute absolute z values for each values
  absolute_z <- abs(dat - mean(dat, na.rm = T)) / stdev

  ## Subset data that has absolute_z greater than the zCutOff
  ## ...and replace them with replace
  ## Can also replace with other values (such as max/mean of data)
  outlier <- rep(F, length(dat))
  outlier[absolute_z > zCutOff] <- T

  ## If values == TRUE, return z score for each value
  if (returnZValues) {
    return(absolute_z) #if values == TRUE, return z score for each value

    ## Otherwise, return values with outliers replaced
  } else {
    #return(round(dat, digits))
    return(outlier)
  }

}


#' @export
labelOutlierZ <- codeOutliersZ

