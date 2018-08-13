#' @title Load libraries from libraries file
#' @export
attachLibraries <- function(file_path) {

  libs <- readLines(file_path)

  for(x in libs) {
    pos <- gregexpr("#", x)[[1]][1]

    if (pos == -1) {
      pos = nchar(x) + 1
    }

    res <- substr(x, 1, pos-1)
    res <- sub(" ", "", res)

    if (res != "") {
      require(res, character.only = T)
    }
    outputString(paste("* Attached package:", res))
  }
}
