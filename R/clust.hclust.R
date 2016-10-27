#' @title Hierarchical clustering
#' @export
clust.hclust <- function (dat_wide,
                          meth4dist = "EUCL",
                          meth4agglo = "single",
                          k = 3,
                          ...) {

  outputFunProc(R)
  outputString(paste("* Dissimilartiy measure:", meth4dist))
  outputString(paste("* Agglomeration method:", meth4agglo))
  outputString(paste("* Cutree:", k))

  ## Compute distance matrix using package TSclust
  #data2clust_wide_t <- t(dat_wide)
  outputString("* Computing distance matrix ...")
  distmat <- TSclust::diss(as.matrix(dat_wide), method4dist)
  outputDone(step = T)

  ## Run algorithm
  outputString("* Running hclust algorithm ...")
  res <- hclust(distmat, method4agglo)
  outputDone(step = T)

  ## New data.frame for ids and corresponding cluster-group
  assignment <-
    data.frame(id = rownames(dat_wide),
               clustgroup = factor(cutree(res, k = k)),
               row.names  = NULL)

  outputDone()
  return(list(result = res, assignment = assignment))
}
