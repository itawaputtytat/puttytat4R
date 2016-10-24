#' @title Print currently used function name and type
#' @description Print currently used function name and type
#' @param pkg_name Package name as object name or character
#' @param character.only logical. default = FALSE (function will deparse object name to character)
#' @export

reloadPackage <- function(name4pkg,
                          character.only = FALSE) {

  ## In case name4pkg is no string
  if (!character.only) name4pkg <- deparse(substitute(name4pkg))

  ## Detach package
  detachPackage(name4pkg, character.only = T)

  ## Re-attach package
  library(name4pkg, character.only = T)
  outputWSepLine(paste("Package attached:", name4pkg))
}
