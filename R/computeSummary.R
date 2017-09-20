#' @title Compute summary
#' @export
computeSummary <- function(dat, col_name_group, col_name_var, fun_name_stat) {

  ## Create final data frame for merging
  dat_final <-
    dat %>%
    group_by_(.dots =col_name_group) %>%
    summarize()

  for (var in col_name_var) {
    for (stat in fun_name_stat) {

      ## Create formula
      formula_temp <- as.formula(paste0("~", stat, "(v)"))

      dat_temp <-
        dat %>%
        group_by_(.dots = col_name_group) %>%
        summarize_(.dots =
                     setNames(list(interp(formula_temp, v = as.name(var) )),
                              unclass(paste(var, stat, sep = "_")))
        )

      dat_final <-
        left_join(dat_final,
                  dat_temp)
    }
  }
  return(dat_final)
}
