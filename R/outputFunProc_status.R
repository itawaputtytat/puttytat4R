#' @title Set outputFunProc active
#' @description Set outputFunProc active
#' @param status logical. default = T (active)
#' @export
.outputFunProc_status <- function(status = F, print = F) {
  if (status) {
    puttytat4R_env$outputFunProc_status <- T
    if(print)
      messageWithSepLine("outputFunProc active", sepline_char = "=")
  } else {
    puttytat4R_env$outputFunProc_status <- F
    if(print)
      messageWithSepLine("outputFunProc inactive", sepline_char = "=")
  }
}
