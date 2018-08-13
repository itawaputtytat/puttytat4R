#' @title Recode/Invert items
#' @description Recodes items from questionnaires
#' @param dat Data object
#' @param name4src Name of database source as character
#' @param range Range of scale as integer
#' @export
recodeItems <- function(dat,
                        col_names,
                        min,
                        max) {

  dat[, col_names] <- max - (dat[, col_names] - min)
  return(dat)

}


#
# dat_long <- reshapeLong(dat, col_names_to_keep = "subject_id", "group")
# dat_long_summary <- computeSummary(dat, "group")


# recodeItems <- function(dat,
#                         col_names_to_invert,
#                         min_actual,
#                         min_target,
#                         max_target) {
#
#   dat[, col_names_to_invert] <-
#     lapply(col_names_to_invert, function(x) {
#       #range - dat[, x] + 1)
#       dat_temp <- dat[, x]
#       dat_temp <- dat_temp + (min_target - min_actual)
#       dat_temp <- (max_target - min_target) - dat_temp
#     })
#
#   return(dat)
# }
