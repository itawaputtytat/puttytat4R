#' @title Plot item profile
#' @export
plotItemProfile <- function(dat,
                            col_name_items = "item_id",
                            col_name_values = "response_mean",
                            col_name_group,
                            value_range) {

  ## Convert to R base data format
  dat <- data.frame(dat)

  ## Reverse items (result: from top to bottom)
  unique_items <- unique(dat[, col_name_items])
  unique_items <- unlist(unique_items, use.names = F)
  unique_items <- sort(unique_items)

  dat[, col_name_items] <-
    factor(dat[, col_name_items], levels = rev(unique_items))

  ## Plot
  plot_dat <-
    ggplot() +
    geom_line(data = dat,
              aes_string(x = col_name_items,
                         y = col_name_values,
                         group = col_name_group,
                         color = col_name_group)) +
    coord_flip(ylim = value_range)

  return(plot_dat)
}
