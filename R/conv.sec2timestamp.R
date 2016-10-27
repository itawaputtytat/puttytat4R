#' @title Convert seconds to timestamp (HH:MM:SS.ms)
#' @param seconds numeric. Seconds
#' @export
conv.sec2timestamp <- function(seconds) {
  outputFunProc(R)
  timestamp <- format(.POSIXct(seconds, tz = "GMT"), "%H:%M:%OS3")
  outputDone()
  return(timestamp)
}
