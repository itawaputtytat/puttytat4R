#' @title Adjust left margin of ggplot2 object to specific margin
#' @description
#' Function creates dummy plot with specific width for left margin, by creating arbitrary y values: Repetition of character "A". Number of repetion can be specified.
#' The resulting width for left in dummy plot will be extracted using ggtable_build().
#' The grobs of target plot object will be adjustet using ggtable_build(). The width for left margin of the dummy plot will overwrite the width for left margin of the target plot.
#' @param plotdat ggplot2 object
#' @param nchar4ylabels integer. Number of "A" character y values. Will result in changing width for left margin.
#' @export
adjustLeftMargin <- function (plotdat, chars_n = 10) {

  outputFunProc(R)

  ## Create dummy data and plot
  dummydat <- data.frame(x = 0, y = paste(rep("A", chars_n), collapse = ""))
  plotdat_dummy <- ggplot(dummydat, aes(x = x, y = y)) + geom_point()

  ## Access grobs change width of target plot
  plotdat_dummy <- ggplot_gtable(ggplot_build(plotdat_dummy))
  plotdat <- ggplot_gtable(ggplot_build(plotdat))
  plotdat$widths[2:3] <- plotdat_dummy$widths[2:3]

  return(plotdat)
}
