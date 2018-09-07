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
                               "se",
                               "ci"),
                           conf = .95,
                           remove_na = T) {

  if (is.null(col_names_group)) {
    dat[, "no_group"] <- 1
    col_names_group <- c("no_group")
  }

  dat_coll <- c()

  for (var in col_names_values) {

    # ## Create final data frame for merging
    dat_final <-
      dat %>%
      distinct_(col_names_group)
    # group_by_(.dots = col_names_group) %>%
    # summarize()

    for (stat in fun_name_stat) {

      ## Create formula
      if (!stat %in% c("se", "ci")) {
        formula_temp <-
          as.formula(paste0("~", stat, "(v, na.rm = remove_na)"))
      } else {

        if (stat == "se") {
          formula_temp <-
            as.formula(paste0("~ sd(v, na.rm = remove_na) / sqrt(n())"))
        }

        if (stat == "ci") {
          formula_temp <-
            as.formula(paste0("~ (sd(v, na.rm = remove_na) / sqrt(n())) * ",
                              "qt(c/2 + .5, n() - 1)"))
        }

      }
      dat_temp <-
        dat %>%
        group_by_(.dots = col_names_group) %>%
        summarize_(.dots = setNames(
          list(interp(formula_temp,
                      v = as.name(var),
                      remove_na = remove_na,
                      c = conf)),
          stat)
        )

      ## Do silently to avoiding joining messages in loop
      suppressMessages(
        dat_final <-
          left_join(dat_final,
                    dat_temp) %>%
          data.frame()
      )
    }
    if (length(col_names_values) > 1) {
      dat_coll[[var]] <- dat_final
    } else {
      dat_coll <- dat_final
    }
  }

  return(dat_coll)
}
