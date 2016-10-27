#' @title Convert timestamp (HH:MM:SS.ms) to seconds
#' @param timestamp character. Timestamp in format HH:MM:SS.s
#' @export
conv.timestamp2sec <- function(timestamp) {

  outputFunProc(R)

  ## Remove last both values of timestamp: ms
  ## ... and convert into POSIX
  t_str <- substr(timestamp, 1, 8)
  t_str <- strptime(timestamp, "%H:%M:%S")

  ## Remove all but ms
  ms <- substr(timestamp, 10, nchar(timestamp))
  ms <- as.numeric(ms) / 10^nchar(ms)

  ## Compute seconds using hours, minutes and
  seconds <-
    as.numeric(format(t_str, "%H")) * 60 * 60 +
    as.numeric(format(t_str, "%M")) * 60 +
    as.numeric(format(t_str, "%OS")) +
    ms

  outputDone()

  return (seconds)
}
