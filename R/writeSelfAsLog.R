#' @title Save currect script as script in log directory
#' @description E.g. for using as backup of query settings
#' @export
writeSelfAsLog <- function(filename) {
  outputFunProc(R)

  ## Get directoy of executed script
  filepath <- dirname(sys.frame(1)$ofile)

  ## Create log directory
  name4dir <- paste("_log_", filename, sep = "")
  dir.create(file.path(filepath, name4dir), showWarnings = FALSE)

  ## Create new filename and path
  filename <- paste(format(Sys.time(), "%y%m%d_%H%M%S"), "_", filename, ".R", sep = "")
  filepath <- file.path(filepath, name4dir, filename)

  ## Save file
  file.copy(sys.frame(1)$ofile, filepath)

  outputDone()
}
