#' @title detachPackage
#' @description This function allows you unload packages quily by name (or as as string)
#' Source: http://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
#' @param pkg_name Package name as object name or character
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

detachPackage <- function(pkg_name, character.only = FALSE, force = FALSE) {

  ## In case pkg_name is no string
  if (!character.only) {
    pkg_name <- deparse(substitute(pkg_name))
  }

  ## Create string for detach function
  search_pkg <- paste("package", pkg_name, sep = ":")

  ## Check if package is in search path for R objects
  if (!search_pkg %in% search())
    outputWSepLine( paste("Package is not attached:", pkg_name) )
  while(search_pkg %in% search()) {
    detach(search_pkg,
           unload = TRUE,
           character.only = TRUE,
           force = force)
    outputWSepLine( paste("Package detached:", pkg_name) )
  }
}
