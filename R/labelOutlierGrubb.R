#' @title Flag outlier according to Grubb (1969)
#' @description http://daryavanichkina.com/wikipages/r/outlierdetectionR.html
#' @export

codeOutlierGrubb <- function(dat) {

  #printFunProc(R)
  library(outliers)

  ## Collector variable
  outliers <- NULL

  testdat <- dat

  ## Perform Grubbs test and extract p-value
  grubbs_result <- grubbs.test(testdat)
  p <- grubbs_result$p.value

  # throw an error if there are too few values for the Grubb's test
  if (length(testdat) < 3 )
    stop("Grubb's test requires > 2 input values")

  ## Repeat Grubbs test
  while (p < 0.05) {

    ## Extract outlier values
    ## e.g. "highest value 6 is an outlier"
    outliers <-
      c(outliers,
        as.numeric( strsplit(grubbs_result$alternative, " ")[[1]][3]) )

    ## Create new dataset without identied outlier
    testdat <- dat[!dat %in% outliers]

    # stop if all but two values are flagged as outliers
    if (length(testdat) < 3 ) {
      warning("All but two values flagged as outliers")
      break
    }

    ## ... otherwise: Perform Grubbs test again
    grubbs_result <- grubbs.test(testdat)
    p <- grubbs_result$p.value
  }
  return(data.frame( data = dat, outlier = dat %in% outliers) )
}


labelOutlierGrubb <- codeOutlierGrubb
