#' @title Set outputFunProc active
#' @description Set outputFunProc active
#' @param status logical. default = T (active)
#' @export

.outputFunProc_status <- function(status = T) {
  if (status) {
    puttytat4R_env$outputFunProc_status <- T
    outputWSepLine("outputFunProc active")
  } else {
    puttytat4R_env$outputFunProc_status <- F
    outputWSepLine("outputFunProc inactive")
  }
}
