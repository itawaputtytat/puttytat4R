#' @title Set printFunProc active
#' @description Set printFunProc active
#' @param status logical. default = T (active)
#' @export

.printFunProc_status <- function(status = T) {
  if (status) {
    puttytat4R_env$printFunProc_status <- T
    outputWSepLine("printFunProc active")
  } else {
    puttytat4R_env$printFunProc_status <- F
    outputWSepLine("printFunProc inactive")
  }
}
