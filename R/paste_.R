#' @title Concatenate strings using underscore as operator
#' @export
paste_ <- function(...) {

  ## Get function call arguments
  input <<- as.list(match.call(expand.dots = FALSE))["..."]
  input <<- do.call(c, unlist(input, recursive=FALSE, use.names = F), env = parent.frame())

  output <- paste(input, collapse = "_")

  return(output)
}

# paste_(sett_clust$algorithm, "HALLO")
#
#
#
#
# testfun <- function(...) {
#   paste_(...)
#   #paste(...)
# }
#
# testfun2 <- function(...) {
#   testfun(...)
# }
#
# testfun2(sett_clust$algorithm, "test2")
#


# paste_ <- function(...) {
#
#   ## Get function call arguments
#   input <- as.list(match.call(expand.dots = FALSE))["..."]
#
#   ## Evaluate non-character objects
#   input <- vapply(unlist(input), function(x) {
#     if ( !is.character(x) ) {
#
#     }
#       eval(x, env = parent.frame()) else
#         x
#   }, character(1))
#
#   ## Collapse vector
#   paste(input, collapse = "_")
# }
