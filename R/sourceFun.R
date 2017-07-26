#' @titleFind Source functions
#' @export
sourceFun <- function(fun_name, print_dir = F) {
  result <- list.files(list.dirs(), fun_name, recursive = T)
  if (length(result) != 0) {
    path <- result[grep("/", result, fixed = T)]
    source(path)
    if (print_dir) {
      outputFunProc(I, fun_name, path)
    } else {
      outputFunProc(I, fun_name)
    }

  } else {
    outputString("Function could not be found")
  }
}
