#' @title k-Means clustering
#' @export
clust.kmeans <-   function (dat_wide,
                            k = 3,
                            iter.max = 500,
                            nstart = 1,
                            algorithm = "Hartigan-Wong",
                            ... ) {

  outputString(paste("* Centers:", k))
  outputString(paste("* Max. Iterations:", iter.max))
  outputString(paste("* nstart:", nstart))
  outputString(paste("* Algorithm:", algorithm))

  ## Run algorithm ----
  outputString("* Running kmeans algorithm ...")
  res <- kmeans(dat_wide,
                centers   = k,
                iter.max  = iter.max,
                nstart    = nstart,
                algorithm = algorithm)
  outputDone(step = T)

  ## Create dataframe for identifier (e.g. passing)
  ## ... and corresponding cluster-nr found using algorithm
  assignment <-
    data.frame(id = rownames(dat2proc.wide),
               clustgroup = factor(res$cluster),
               row.names  = NULL)

  outputDone()
  return(list(result = res, assignment = assignment))
}
