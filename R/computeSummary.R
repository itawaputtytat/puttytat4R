#' @title Compute summary
#' @export
computeSummary <- function(dat,
                           col_names_group = NULL,
                           col_names_values = "value",
                           fun_name_stat =
                             c("mean",
                               "sd",
                               "median",
                               "min",
                               "max",
                               "se"),
                           remove_na = T) {

  if (is.null(col_names_group)) {
    col_names_group <- c("group", "variables")
  }

  ## Create final data frame for merging
  dat_final <-
    dat %>%
    group_by_(.dots = col_names_group) %>%
    summarize()

  for (var in col_names_values) {
    for (stat in fun_name_stat) {

      ## Create formula
      if (!stat == "se") {
        formula_temp <-
          as.formula(paste0("~", stat, "(v, na.rm = remove_na)"))
      } else {
        formula_temp <-
          as.formula(paste0("~ sd(v, na.rm = remove_na) / sqrt(n())"))
      }

      dat_temp <-
        dat %>%
        group_by_(.dots = col_names_group) %>%
        summarize_(.dots = setNames(
          list(interp(formula_temp,
                      v = as.name(var) )),
          stat)
        )

      ## Do silently to avoiding joining messages in loop
      suppressMessages(
        dat_final <-
          left_join(dat_final,
                    dat_temp)
      )
    }
  }

  dat_final <- data.frame(dat_final)

  return(dat_final)
}

