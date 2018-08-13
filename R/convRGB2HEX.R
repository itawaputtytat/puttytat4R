#' @title Convert RGB to HEX
#' @export
convRGB2Hex <- function(r, g, b, a = 1) {
  hex <- rgb(r, g, b, a*255, maxColorValue = 255)
  return(hex)
}
