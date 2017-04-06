#' @title Recode/Invert items
#' @description Recodes items from questionnaires
#' @param dat Data object
#' @param name4src Name of database source as character
#' @param range Range of scale as integer
#' @export
recodeItems <- function(dat, name4cols, range) {
  dat[, name4cols] <-
    lapply(name4cols, function(x)
      range - dat[, x] + 1)
  return(dat)
}
