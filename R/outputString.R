#' @title Print text with enclosing separating lines
#' @description Print multiple character values with enclosing separating lines
#' @param string Vector of character values
#' @param output_sepline Boolean value for printing sepline. Default: False
#' @param sepline_pos Vector of character values. Position of separating lines ("top", "t", "T", "bottom", "b", "B"). Default: NULL, Default when output_sepline = True: c("T", "B")
#' @param sepline_char Character value for separating line. Default: "-"
#' @param return_char Return output as string without printing the output. Default = False
#' @export
outputString <- function(string,
                         type = "cat",
                         output_sepline = F,
                         sepline_pos = NULL, #c("T", "B")
                         sepline_char = "-",
                         return_char = F) {

  ## Default output
  output <- paste(string, collapse = "\n")

  ## If sepline_pos has been set:
  ## Change output_sepline to True
  ## Extract initial letter of sepline_pos entrys and translate to lowercase
  if (!is.null(sepline_pos)) {
    output_sepline <- T
    vapply(sepline_pos,
           function(x) substr(tolower(x), 1, 1),
           character(1),
           USE.NAMES = F)
  }

  ## If output_sepline has been set, but sepline_pos is empty: Set "T" and "B"
  if (output_sepline & is.null(sepline_pos))
    sepline_pos <- c("t", "b")

  ## If output_sepline has been set (Default: F)
  if (output_sepline) {

    ## Get maximum number of characters needed for length of separating line
    ## Create separating line
    output_length <- max(sapply(string, nchar))
    sepline <- paste(rep(sepline_char, output_length), collapse = "")

    ## Merge separating line and output string depending on sepline_pos
    if (sepline_pos[1] %in% c("t") & length(sepline_pos) == 1)
      output <- paste(sepline, paste(output, collapse = "\n"), sep = "\n")
    if (sepline_pos[1] %in% c("b") & length(sepline_pos) == 1)
      output <- paste(paste(output, collapse = "\n"), sepline, sep = "\n")
    if (length(sepline_pos) == 2)
      output <- paste(sepline, paste(output, collapse = "\n"), sepline, sep = "\n")
  }

  ## If return_char, return chat without printing output
  if (return_char) {
      return(output)
  } else {
      if (type == "cat") cat(output, "\n")
      if (type == "message") message(output)
  }
}
