#' @title Compute ECDF
#' @export
computeECDF <- function(dat) {
  n = length(dat)
  x = sort(dat)
  y = seq(1, n) / n
  return(data.frame(x = x, y = y))
}
