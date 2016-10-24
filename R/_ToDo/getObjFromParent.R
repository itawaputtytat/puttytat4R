level1 <- function(x1) {
  outputFunProc(R)
  cat("Level 1: Current frame is", sys.nframe(), "\n")
  cat("Level 1: Objects in current frame are: \n")
  print(ls())
  cat("Level 1: Objects in parent 1 are: \n")
  print(ls(envir = sys.frame(sys.nframe() - 1))) ## Print objects in level above
  ## Which is the same as print(ls(envir = parent.frame())) ## Print objects in level above
  level2(x1)
}

level2 <- function(x2) {
  outputFunProc(R)
  cat("Level 2: Current frame is", sys.nframe(), "\n")
  cat("Level 1: Objects in current frame are: \n")
  print(ls())
  cat("Level 2: Parents are", sys.parents(), "\n")
  #print(sys.function(-1)) ## Print function above
  cat("Level 2: Objects in parent 1 are: \n")
  print(ls(envir = sys.frame(-1))) ## Print objects in level above
  level3(x2)
  }

level3 <- function(x3) {
  outputFunProc(R)
  cat("Level 3: Current frame is", sys.nframe(), "\n")
  cat("Level 3: Objects in current frame are: \n")
  print(ls())
  cat("Level 3: Parents are", sys.parents(), "\n")
  #print(sys.function(-1)) ## Print function above
  cat("Level 3: Objects in parent 1 are: \n")
  print(ls(envir = sys.frame(-1))) ## Print objects in level above
  cat("Level 3: Objects in parent 2 are: \n")
  print(ls(envir = sys.frame(-2))) ## Print objects in level above
  #print(ls(envir = parent.frame(n = 2))) ## Print objects in level above
  cat("Level 3: Objects in parent 3 are: \n")
  print(ls(envir = sys.frame(-3))) ## Print objects in level above
  cat("Level 3: Print name of argument: \n")
  dat2proc_name <- ls(envir = sys.frame(-2))[1]
  print(dat2proc_name)
  cat("Level 3: Print content of argument: \n")
  #arg_content <- get(dat2proc_name, envir = sys.frame(-2))
  arg_content <- eval( parse("deparse(substitute(", ls(envir = sys.frame(-2))[1], "))"), envir = sys.frame(-2) )
  print(arg_content)
  # arg <-
  # arg_exists <- exists(arg, .GlobalEnv)
  # cat("Level 3: Does object exist? \n")
  # print(arg_exists)
  ## In case arg is already a character value remove quoting strings
  ## .. resultig from deparse(substitute(arg))
  # if(grepl("\"", arg)) {
  #   arg <- substr(arg, 2, nchar(arg) - 1)
  #   if(arg_exists)
  #     cat("*Character", "\n", "*Object exists", sep = "") else
  #       cat("*Character", "\n", "*Object does not exist", sep = "")
  # } else {
  #   if(arg_exists)
  #       cat("*No character", "\n", "*Object exists", sep = "") else
  #         cat("*Character", "\n", "*Object does not exist", sep = "")
  # }
}













#
#
# ## Note: the first two examples will give different results
# ## if run by example().
# ff <- function(x) gg(x)
# gg <- function(y) sys.status()
# str(ff(1))
#
# gg <- function(y) {
#   ggg <- function() {
#     cat("current frame is", sys.nframe(), "\n")
#     cat("parents are", sys.parents(), "\n")
#     print(sys.function(0)) # ggg
#     print(sys.function(2)) # gg
#   }
#   if(y > 0) gg(y-1) else ggg()
# }
# gg(3)
#
#
# t1 <- function() {
#   aa <- "here"
#   t2 <- function() {
#     ## in frame 2 here
#     cat("current frame is", sys.nframe(), "\n")
#     str(sys.calls()) ## list with two components t1() and t2()
#     cat("parents are frame numbers", sys.parents(), "\n") ## 0 1
#     print(ls(envir = sys.frame(-1))) ## [1] "aa" "t2"
#     invisible()
#   }
#   t2()
# }
# t1()
