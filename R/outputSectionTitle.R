#' @title Output section title
#' @export

outputSectionTitle <- function(text,
                               char_aes = "*",
                               sepline_char = "=",
                               ...) {

  ## Create string for text
  aes <- paste(rep(char_aes, 3), collapse = "")
  title_part1 <- paste(aes, paste(rep(" ", 1), collapse = ""), sep = "")
  title_part2 <- paste(rev(strsplit(title_part1, NULL)[[1]]), collapse = "")
  length_title_part1_length <- nchar(title_part1)
  title <- paste(title_part1, text, title_part2)

  catWithSepLine(title, sepline_char = sepline_char, ...)
}
