#' @title Test functions for outputFunProc
#' @description Print multiple character values with enclosing separating lines

#' @export
.outputFunProc_test <- function() {
  outputFunProc(R)
  calls <- as.character(sys.calls())
  #frames <-
  cat(calls, sep = "\n")
  #cat(sys.frames(), sep = "\n")
}

#' @export
.outputFunProc_test_emb <- function(...) {
  outputFunProc(R)
  .outputFunProc_test()
}

#' @export
.outputFunProc_test_emb2 <- function(...) {
  outputFunProc(R)
  .outputFunProc_test_emb()
}
