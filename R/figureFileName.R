#' @title Create file name for figures
#' @export
figureFileName <- function(name, ext = ".png") {
  name <- paste_(format(Sys.Date(), format="%y%m%d"), name)
  name <- paste0(name, ext)
  return(name)
}
