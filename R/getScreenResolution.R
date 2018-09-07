#' @export
getScreenResolution <- function() {

  ## Extract screen resolution
  width <- system("wmic desktopmonitor get screenwidth", intern=TRUE)
  height <- system("wmic desktopmonitor get screenheight", intern=TRUE)

  ## Extract numeric values
  width <- as.numeric(width[-c(1, length(width))])
  height <- as.numeric(height[-c(1, length(height))])

  ## Remove NA
  width <- width[!is.na(width)]
  height <- height[!is.na(height)]

  return (resolution = list(width = width, height = height))
}
