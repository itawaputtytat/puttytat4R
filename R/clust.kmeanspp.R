#' @title k-Means++ clustering
#' @export
clust.kmeanspp <-   function (dat_wide,
                              k = 3,
                              start = "random",
                              iter.max = 500,
                              nstart = 1,
                              algorithm = "Hartigan-Wong",
                              ...) {

  outputFunProc(R)
  outputString(paste("* Centers:", k))
  outputString(paste("* Start:", start))
  outputString(paste("* Max. Iterations:", iter.max))
  outputString(paste("* nstart:", nstart))
  outputString(paste("* Algorithm:", algorithm))

  ## Run algorithm using kmeanspp from LICORS
  outputString("* Running kmeanspp algorithm ...")
  res <- LICORS::kmeanspp(dat_wide,
                          k = k,
                          start = start,
                          iter.max = iter.max,
                          nstart = nstart,
                          algorithm = algorithm)
  outputDone(step = T)

  ## Create dataframe for identifier (e.g. passing)
  ## ... and corresponding cluster-nr found using algorithm
  assignment <-
    data.frame(id = rownames(dat_wide),
               clustgroup = factor(res$cluster),
               row.names  = NULL)

  outputDone()
  return(list(result = res, assignment = assignment))
}
