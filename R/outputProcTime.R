#' @title Output processing time
#' @param ptm Must be initialised using proc.time()
#' @export
outputProcTime <- function(ptm) {
  outputFunProc(R)
  time <- round((proc.time() - ptm)[3], 2)
  timestamp <- conv.sec2timestamp(time)
  outputString(paste("* Overall time: ", time, " s (", timestamp, ")", sep = ""))
}
