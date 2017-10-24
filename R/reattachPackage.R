#' @export
reattachPackage <- function(package_name = "puttytat4R", ...) {
  detachPackage(package_name, ...)
  library(package_name, character.only = T)
  messageWithSepLine(paste("Re-attached library:", package_name))
}
