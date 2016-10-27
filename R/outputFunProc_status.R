#' @title Set outputFunProc active
#' @description Set outputFunProc active
#' @param status logical. default = T (active)
#' @export
.outputFunProc_status <- function(status = T, print = F) {
  if (status) {
    puttytat4R_env$outputFunProc_status <- T
    if(print)
      messageWSepLine("outputFunProc active", seplinechar = "=")
  } else {
    puttytat4R_env$outputFunProc_status <- F
    if(print)
      messageWSepLine("outputFunProc inactive", seplinechar = "=")
  }
}
