#' @titleFind Read R code from a File and Output
#' @export
sourceWithEcho <- function(path) {

  ## Extract filename
  pos <- regexpr("/", path)
  pos <- max(pos)
  filename <- substr(path, pos + 1, nchar(path))

  ## Source and output
  source(path)
  outputFunProc(I, filename)
}
