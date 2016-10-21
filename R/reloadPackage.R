#' @title Print currently used function name and type
#' @description Print currently used function name and type
#' @param pkg_name Package name as object name or character
#' @param character.only logical. default = FALSE (function will deparse object name to character)
#' @export

reloadPackage <- function(pkg_name, character.only = FALSE) {

  ## In case pkg_name is no string
  if (!character.only) {
    pkg_name <- deparse(substitute(pkg_name))
  }

  ## Detach package
  outputWSepLine( paste("Package detached:", pkg_name) )

  ## Re-attach package
  library( pkg_name, character.only = T)
  outputWSepLine( paste("Package attached:", pkg_name) )
}
