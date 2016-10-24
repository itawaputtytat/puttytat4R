#' @title Interpolate numeric values
#' @export

intrplNum <- function(values, rows_w_dat) {
  outputFunProc(R)
  ## Remeber number of decimal places
  decplaces_n <- getDecimalPlaces(values[rows_w_dat], output = F)
  ## Use na.approx from zoo package for linear interpolation
  values <- na.approx(values, na.rm = F)
  ## Round to original number of decimal places
  values <- round(values, decplaces_n)
  return(values)
}
