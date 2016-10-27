#' @title Print text with enclosing separating lines
#' @description Print multiple character values with enclosing separating lines
#' @param string Vector of character values
#' @param position Vector of character values. Position of separating lines ("top", "bottom")
#' @param sepchar Character value for separating line
#' @export
outputString <- function(string,
                         type = "cat",
                         sepline = F,
                         seplinepos = c("T", "B"),
                         seplinechar = "-",
                         returnchar = F) {

  if (sepline) {
    ## Get maximum number of characters as length for separating line
    output_length <- max(sapply(string, nchar))

    ## Create separating line and final output
    sepline <- paste(rep(seplinechar, output_length), collapse = "")

    if (seplinepos[1] %in% c("t", "T"))
      output <- paste(sepline, paste(string, collapse = "\n"), sep = "\n")
    if (seplinepos[1] %in% c("b", "B"))
      output <- paste(paste(string, collapse = "\n"), sepline, sep = "\n")
    if (sum(seplinepos %in% c("t", "T", "b", "B")) == 2)
      output <- paste(sepline, paste(string, collapse = "\n"), sepline, sep = "\n")
    output <- paste("\n", output, "\n", sep = "")
  } else {
    output <- paste(string, collapse = "\n")
  }

  if (returnchar)
    return(output) else {
      if (type == "message") message(output)
      if (type == "cat") cat(output, "\n")
    }
}

#' @export
messageWSepLine <- function(string, ...)
  outputString(string, type = "message", sepline = T, ...)

#' @export
catWSepLine <- function(string, ...)
  outputString(string, type = "cat", sepline = T, ...)
