#' @title Print text with enclosing separating lines
#' @description Print multiple character values with enclosing separating lines
#' @param output_txt Vector of character values
#' @param position Vector of character values. Position of separating lines ("top", "bottom")
#' @param sepchar Character value for separating line
#' @export
outputWSepLine <- function(output_txt,
                           type = "message",
                           pos = c("top", "bottom"),
                           sepchar = "-",
                           returnchar = F) {

  ## Get maximum number of characters as length for separating line
  output_length <- max(sapply(output_txt, nchar))

  ## Create separating line and final output
  sepline <- paste(rep(sepchar, output_length), collapse = "")

  if (pos[1] == "top")
    output_final <-
    paste(sepline, "\n", paste(output_txt, collapse = "\n"), sep = "\n")

  if (pos[1] == "bottom")
    output_final <-
    paste(paste(output_txt, collapse = "\n"), sepline, sep = "\n")

  if ( sum(pos %in% c("top", "bottom")) == 2 )
    output_final <-
    paste(sepline, paste(output_txt, collapse = "\n"), sepline, sep = "\n")

  if (returnchar)
    return(output_final) else {
      if (type == "message") message(output_final)
      if (type == "cat") cat(output_final)

    }


}

#' @export
messageWSepLine <- function(output_txt) {
  outputWSepLine(output_txt, type = "message")
}

#' @export
catWSepLine <- function(output_txt) {
  outputWSepLine(output_txt, type = "cat")
}
