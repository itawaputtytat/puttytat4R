#' @title: Output function call stack
#' @description Outputs current frame number and objects, as well as frame number and objects from parents
#' @export
outputFunCallStack <- function() {
  outputFunProc(R)
  framenr_now <- sys.nframe()
  objlist_now <- ls()
  outputString(paste("* Current frame nr:", framenr_now))
  outputString("* Objects in current frame:"); print(objlist_now)
  for (frame in 0 : (framenr_now - 1) ) {
    cat("\n")
    objlist <- ls(envir = sys.frame(sys.nframe() - 1))
    ## Which is the same as print(ls(envir = parent.frame())) ## Print objects in level above
    outputString(paste("* Parent frame nr:", frame))
    outputString("* Objects:"); print(objlist_now)
  }
}
