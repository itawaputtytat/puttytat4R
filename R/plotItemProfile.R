#' @title Plot item profile
#' @export
plotItemProfile <- function(dat,
                            col_name_items = "variables",
                            col_name_values = "mean",
                            col_name_group = NULL,
                            value_range) {

  ## Convert to R base data format
  dat <- data.frame(dat)

  if (is.null(col_name_group)) {
    col_name_group = "no_group"
    dat[, col_name_group] <- col_name_group
  }

  ## Reverse items (result: from top to bottom)
  unique_items <- unique(dat[, col_name_items])
  # unique_items <- unlist(unique_items, use.names = F)
  # unique_items <- sort(unique_items)

  dat[, col_name_items] <-
    factor(dat[, col_name_items], levels = rev(unique_items))

  ## Plot
  plot_dat <-
    ggplot() +
    geom_line(data = dat,
              aes_string(x = col_name_items,
                         y = col_name_values,
                         group = col_name_group,
                         color = col_name_group,
                         linetype = col_name_group,
                         size = col_name_group)) +
    geom_line(data = dat,
              aes_string(x = col_name_items,
                         y = col_name_values,
                         group = col_name_group,
                         color = col_name_group,
                         alpha = col_name_group)) +
    geom_point(data = dat,
              aes_string(x = col_name_items,
                         y = col_name_values,
                         group = col_name_group,
                         color = col_name_group,
                         shape = col_name_group,
                         col_name_group)) +
    coord_flip(ylim = value_range)

  return(plot_dat)
}
