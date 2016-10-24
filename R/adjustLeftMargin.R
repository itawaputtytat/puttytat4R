#' @title Adjust left margin of ggplot2 object to specific margin
#' @description
#' Function creates dummy plot with specific width for left margin, by creating arbitrary y values: Repetition of character "A". Number of repetion can be specified.
#' The resulting width for left in dummy plot will be extracted using ggtable_build().
#' The grobs of target plot object will be adjustet using ggtable_build(). The width for left margin of the dummy plot will overwrite the width for left margin of the target plot.
#' @param plotdat ggplot2 object
#' @param nchar4ylabels integer. Number of "A" character y values. Will result in changing width for left margin.
#' @export
adjustLeftMargin <- function (plotdat, nchar4ylabels = 10) {

  #require(gridExtra)

  ## Create dummy data
  dummydat <-
    data.frame(xval = 0,
               yval = paste(rep("A", nchar4ylabels), collapse = ""))

  ## Create dummy plot
  plotdat_dummy <-
    ggplot(dummydat, aes(x = xval, y = yval)) +
    geom_point()

  ## Access grobs
  plotdat_dummy <- ggplot_gtable(ggplot_build(plotdat_dummy))
  plotdat <- ggplot_gtable(ggplot_build(plotdat))

  ## Change width of target plot
  plotdat$widths[2:3] <- plotdat_dummy$widths[2:3]

  return(plotdat)
}
