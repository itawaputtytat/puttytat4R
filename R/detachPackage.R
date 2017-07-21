#' @title detachPackage
#' @description This function allows you unload packages quily by name (or as as string)
#' Source: http://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
#' @param name4pkg Package name as object name or character
#' @param character.only logical. default = FALSE (function will deparse object name to character)
#' @param force logical. default = FALSE. If set to TRUE package will be detached even though other attached packages depend on it
#' @keywords detachPackage
#' @examples
#' #install.packages("ggplot2")
#' #library(ggplot2)
#' #unloadPackage(ggplot2)
#' #library(ggplot2)
#' #unloadPackage("ggplot2")
#' @export

detachPackage <- function(name4pkg = "puttytat4R",
                          character.only = FALSE,
                          force = FALSE) {

    ## In case name4pkg is no string
  if (name4pkg != "puttytat4R" & !character.only)
    name4pkg <- deparse(substitute(name4pkg))

  ## Create string for detach function
  search_pkg <- paste("package", name4pkg, sep = ":")

  ## Check if package is in search path for R objects
  if (!search_pkg %in% search())
    outputString(paste("Package is not attached:", name4pkg))
  while(search_pkg %in% search()) {
    detach(search_pkg,
           unload = TRUE,
           character.only = TRUE,
           force = force)
    messageWithSepLine(paste("Detached package:", name4pkg))
  }
}
