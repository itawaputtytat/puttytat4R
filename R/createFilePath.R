
#' @title Build file path and create subdirectories if necessary
#' @export
createFilePath <- function(dir_list) {

  outputFunProc(R)

  ## Initialise first sub directory
  file_path <- file.path(dir_list[1])

  ## Create file path if necessary
  dir.create(file.path(file_path),
             showWarnings = F,
             recursive = T)

  ## Complete file path
  for(dir in dir_list[-1]) {

    file_path <- file.path(file_path, dir)

    ## Create file path if necessary
    if (!file.exists(file_path))
      dir.create(file.path(file_path),
                 showWarnings = F,
                 recursive = T)
  }

  outputDone()

  return(file_path)
}
