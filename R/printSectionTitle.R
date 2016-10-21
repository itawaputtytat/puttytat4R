#' @title Output section title
#' @export

printSectionTitle <- function(text) {

  ## Compute number of character
  outputlength <- nchar(text)

  ## Create string for text
  str4title_part1 <- paste("***", paste(rep(" ", 1), collapse = ""), sep = "")
  str4title_part2 <- paste(rev(strsplit(str4title_part1, NULL)[[1]]), collapse = "")
  str4title_part1_length <- nchar(str4title_part1)
  str4title <- paste(str4title_part1, text, str4title_part2)

  ## Create seperating line
  ## Length = Output length + 2*X asteriks + 2 spaces
  sepline <- paste(rep("*", outputlength + 2 * str4title_part1_length + 2*1), collapse = "")

  ## Create final string for output
  str4output <- paste("", sepline, str4title, sepline, "", sep = "\n")

  ## Print output
  message(str4output)

}
