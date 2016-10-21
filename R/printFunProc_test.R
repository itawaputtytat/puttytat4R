#' @title Test functions for printFunProc
#' @description Print multiple character values with enclosing separating lines

#' @export
.printFunProc_test <- function() {
  printFunProc(R)
}

#' @export
.printFunProc_test_emb <- function() {
  printFunProc(R)
  .printFunProc_test()
}

#' @export
.printFunProc_test_emb2 <- function() {
  printFunProc(R)
  .printFunProc_test_emb()
}
