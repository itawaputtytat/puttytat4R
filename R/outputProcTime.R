#' @title Output processing time
#' @param ptm Must be initialised using proc.time()
#' @export
outputProcTime <- function(ptm, keep_ptm = F) {
  outputFunProc(R)
  time <- round((proc.time() - ptm)[3], 2)
  timestamp <- conv.sec2timestamp(time)
  outputString(paste("* Overall time: ", time, " s (", timestamp, ")", sep = ""))
  name_ptm_arg <- deparse(substitute(ptm))
  rm(list = name_ptm_arg, envir = parent.frame()) ## Removing ptm from parent
}
