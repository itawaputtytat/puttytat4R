#' @export

clearWorkspace <- function() {
  printFunProc(R)
  cat("Resetting R-Environment ... \n")
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  printDone()
}
