#' @title Reshape data to long data frame
#' @export
reshapeLong <- function(dat,
                        col_name_gathered_col_names = "col_name",
                        col_name_gathered_values = "value",
                        col_names_to_keep = NULL,
                        ...) {

  dat_long <-
    dat %>%
    gather_(key = col_name_gathered_col_names,
            value = col_name_gathered_values,
            gather_cols = setdiff(names(dat), col_names_to_keep),
            ...)

  return(dat_long)
}

