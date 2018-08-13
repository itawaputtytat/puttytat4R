#' @export
convertSubscaleToFactor <- function(dat,
                                    levels,
                                    labels,
                                    lang = "eng",
                                    col_name_subscale = "subscale",
                                    col_name_overall_suffix = "overall",
                                    col_name_factor_suffix = "fct") {

  ## Look for values named "overall"
  ## First result is enough
  row_finder <-
    grepl(col_name_overall_suffix,
          dat_bsss_scores_long[, col_name_subscale])
  row_finder <- which(row_finder)[1]

  ## If there is no values named overall
  if (length(row_finder) == 0) {

    levels <- c(levels)
    labels <- rec(labels)

    ## If there is a values names overall
  } else {

    ## Get the name for overall subscale value
    col_name_overall <- dat_bsss_scores_long[row_finder, col_name_subscale]

    ## Name total column
    if (lang == "ger") {
      name_total <- "Gesamt"
    } else {
      name_total <- "Total"
    }

    ## Create levels and labels
    levels <- c(levels, col_name_overall)
    levels <- rev(levels)
    labels <- c(labels, name_total)
    labels <- rev(labels)
  }

  ## Create factor
  col_name <- paste_(col_name_subscale, col_name_factor_suffix)
  dat[, col_name] <- dat[, col_name_subscale]
  dat[, col_name] <-
    factor(dat[, col_name_subscale],
           levels = levels,
           labels = labels)

  return(dat)
}
