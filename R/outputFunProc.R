#' @title Print currently used function name and type
#' @description Print currently used function name and type
#' @param proctype Single character for identifying process type: "I" for "Initialising", "R" for "Running", and "D" for "Done". No default. "I" and "D" can be used with any (arbitrary) function name without constrictions. "R" depends on active and parent calls.
#' @param funname String for function name. If no function name is given, outputFunProc will deparse function name from active call (as outputFunProc can be run inside another function).
#' @export

outputFunProc <- function(proctype, funname = "") {

  ## Print function process only if puttytat4R.env$outputFunProc is TRUE
  ## ... otherwise leave without error message
  if (!puttytat4R_env$outputFunProc_status) {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
  } else {

    ## Get list of active calls
    calls <- as.character(sys.calls())

    ## Check for type of code evaluation (source vs. run)
    if (sum( grepl("eval", calls) ) != 0)
      sourced <- T else
        sourced <- F

      ## Check the number of the current (environmental) frame
      ## (depending on type of code evaluation (source vs. run)
      ## Parameters tested
      nframe <- sys.nframe()
      nframe_now <- nframe - 1
      nframe_prev <- nframe - 2
      if (sourced)
        nframe_user <- nframe_now - 5 else
          nframe_user <- nframe_now - 1

      ## Get input arguments as string
      proctype <- deparse(substitute(proctype))
      if (funname == "") funname <- calls[nframe_now]

      ## Create text for output
      if (proctype == "I") txt4proctype <- "* Function initialised"
      if (proctype == "R") txt4proctype <- "Running"
      if (proctype == "D") txt4proctype <- "Done"
      txt4proctype <- paste(txt4proctype, ":", sep = "")
      txt4output <- c(txt4proctype, funname)

      ## In case of running with parent call (overwrite previous settings)
      if (proctype == "R" & nframe_user != 0) {

        ## Create text for call output
        txt4anglbr <- paste( rep(">", nframe_user), collapse = "")
        txt4call <- paste(txt4anglbr, " Called from:", sep = "")
        txt4call <- c(txt4call, calls[nframe_prev])
        txt4output <- c(txt4output, txt4call)
      }

      messageWSepLine(txt4output)
  }
}

