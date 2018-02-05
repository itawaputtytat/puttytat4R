#' @title Load libraries from libraries file
#' @export
attachLibraries <- function(file_path) {

  libs <- readLines(file_path)

  for(x in libs) {
    pos <- max(0, gregexpr("#", x)[[1]][1])

    if (pos == -1) {
      pos = nchar(x) + 1
    }

    res <- substr(x, 1, pos-1)
    res <- sub(" ", "", res)

    if (res != "") {
      library(res, character.only = T)
    }
  }
}
