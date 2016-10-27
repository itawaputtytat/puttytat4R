#' @title Output section title
#' @export

outputSectionTitle <- function(text,
                               char4aes = "*",
                               seplinechar = "=",
                               ...) {

  ## Compute number of character
  outputlength <- nchar(text)

  ## Create string for text
  aes <- paste(rep(char4aes, 3), collapse = "")
  str4title_part1 <- paste(aes, paste(rep(" ", 1), collapse = ""), sep = "")
  str4title_part2 <- paste(rev(strsplit(str4title_part1, NULL)[[1]]), collapse = "")
  str4title_part1_length <- nchar(str4title_part1)
  str4title <- paste(str4title_part1, text, str4title_part2)

  catWSepLine(str4title, seplinechar = seplinechar, ...)
}
