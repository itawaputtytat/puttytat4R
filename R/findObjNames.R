#' @titleFind objects in workspace
#' @export
findObjNames <- function (txt2incl, txt2excl = NULL, output = F) {

  outputFunProc(R)

  ## Look for names which includes txt2incl
  objnames <- ls(env = .GlobalEnv) ## List objects in workspace
  ## Look for names which includes txt2incl

  if (!is.null(txt2incl))
    for (pattern in txt2incl) {
      finder <- which(grepl(pattern, objnames))
      objnames <- objnames[finder]
    }

  if (!is.null(txt2excl))
    for (pattern in txt2excl) {
      finder <- which(!grepl(pattern, objnames))
      objnames <- objnames[finder]
    }

  if (output) {
    if (length(objnames) > 0) {
      outputString("Found objects of interest:", output_sepline = T, sepline_pos = "b")
      outputString(paste(paste(objnames, collapse = "\n"), "\n"))
    } else outputString("No objects found")
  }
  outputDone()
  return(objnames)
}
