#' @title Return function argument as character
#' @param arg Any value
#' @export

funArgAsChar <- function(arg) {
  outputFunProc(R)
  parent_objlist = ls(envir = sys.frame(1))
  cat("Parent object list: \n")
  print(parent_objlist)
  cat("\n")
  print(eval(substitute(parent_objlist)), envir = sys.frame(1))
  # cat("Type of parent object \n")
  # print(eval(class(parent_objlist)), envir = sys.frame(1))
  # nesty <- function(argy) {
  #   argy <-  deparse(substitute(argy))
  #   if (is.character(argy))
  #     cat("Argument is character") else
  #       cat("Argument is not character")
  # }

  #test1 <- eval.parent(substitute(arg), n = 1)-
  # test1 <- eval.parent(do.call(parse("function(", arg, ") is.character(", arg, ") ")), n = 1)
  # #test <- do.call("substitute", list(arg), envir = parent.frame())
  # cat(test, "\n")
  # print(test1, "\n")
  # cat("\n")
  # cat("sys.frame(-1): \n")
  # test <- ls(envir = sys.frame(-1))
  # cat(test, "\n")
  # cat("\n ... exiting sys.frame(-1) \n")
  # test2 <- eval.parent(parse("deparse(substitute(", arg, "))"), n = 1)
  # cat(test2, "\n")
  # test <- parent.frame()$arg
  # test <- eval.parent(deparse(substitute(arg)), n = 1)
  # #cat(arg, "\n")
  # cat(test, "\n---\n")
  # environment()
  ## Quote argument expression and turn in to character
  arg <- deparse(substitute(arg))
  ## In case arg is already a character value remove quoting strings
  ## .. resultig from deparse(substitute(arg))
  if(grepl("\"", arg)) {
    arg <- substr(arg, 2, nchar(arg) - 1)
    if(exists(arg))
      cat("Character; \n Object exists") else
        if(exists(get(arg, pos = 2)))
          cat("Character; \n Object really exists") else
          cat("Character; \n Object does not exist")
  } else {
    if(exists(arg))
        cat("No character; \n Object exists") else
          if(exists(get(arg, pos = 2)))
            cat("Character; \n Object really exists") else
              cat("Character; \n Object does not exist")
    }
  return(arg)
}
## DOES NOT WORK
