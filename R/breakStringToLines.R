#' @title Break text into new lines by max. characters per laine
#' @export
breakStringToLines <- function(text,
                               max_nchar = NULL,
                               max_nlines = NULL,
                               max_nwords = NULL) {

  ## Catch error if_nchar both threshold arguments are activated
  if (is.null(max_nchar) & is.null(max_nlines) & is.null(max_nwords)) {
    stop("Set either max_nchar, max_nlines, or max_nwords")
  }

  if (!is.null(max_nchar) & !is.null(max_nlines) & !is.null(max_nwords)) {
    stop("Set either max_nchar, max_nlines, or max_nwords")
  }

  ## In case of vector
  if (length(text) > 1) {
    new_labels <-
      sapply(text,
             function(x) {
               x <- as.character(x)
               breakStringToLines(x,
                                  max_nchar = max_nchar,
                                  max_nlines = max_nlines,
                                  max_nwords = max_nwords)
             })
    return(new_labels)
  }

  ## Split text in to separate text elements
  text_split <- unlist(strsplit(text, " "))
  ## Add space after each element
  text_split[1:(length(text_split) - 1)] <-
    paste(text_split[1:(length(text_split) - 1)], "")

  ## Create new string
  text_final <- c()
  nchar_cum <- 0

  if (!is.null(max_nchar)) {
    for(i in 1:length(text_split)) {
      ## Compute the potential new line length
      nchar_cum <- nchar_cum + nchar(text_split[i])
      ## If new line length would exceeds the maximum characters per line
      ## ... insert break
      if ((nchar_cum - 1) > max_nchar) {
        text_final <- paste0(text_final, "\n")
        ## Set new current line length
        nchar_cum <- nchar(text_split[i])
      }
      ## Add new text element
      text_final <- paste0(text_final, text_split[i])
    }
  }

  if (!is.null(max_nlines)) {
    nchar_overall <- nchar(text)
    nchar_per_line <- nchar_overall / max_nlines
    nchar_per_line <- round(nchar_per_line)
    for(i in 1:length(text_split)) {
      ## Add new text element
      text_final <- paste0(text_final, text_split[i])
      ## Compute new line length
      nchar_cum <- nchar_cum + nchar(text_split[i])
      if ((nchar_cum) >= nchar_per_line) {
        text_final <- paste0(text_final, "\n")
        nchar_cum <- 0
      }
    }
  }

  if(!is.null(max_nwords)) {
    for(i in 1:length(text_split)) {
      text_final <- paste0(text_final, text_split[i])
      if (i %% max_nwords == 0 & i!= length(text_split)) {
        text_final <- paste0(text_final, "\n")
      }
    }
  }

  return(text_final)
}

