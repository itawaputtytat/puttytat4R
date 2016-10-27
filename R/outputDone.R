#' @title Output "Done!"
#' @description Outputs "Done!" as c
#' @param
#' @export
#'

outputDone <- function(step = F) {
  if(step) {
    outputString("Done!")
    } else {
      if (puttytat4R_env$outputFunProc_status)
        messageWSepLine("Done!", seplinechar = "=")
    }
}





