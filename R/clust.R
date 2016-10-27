#' @title Use different algorithms for cluster analysis
#' @param dat_wide Data.frame in wide format: Row = case; column = values; multiple columns = e.g. values for each time step
#' @param k integer. Number of groups
#' @param ...
#' @export
clust <- function (dat_wide,
                   k = 3,
                   procedure = "kmeans",
                   algorithm = "Hartigan-Wong",
                   iter.max = 500,
                   nstart = 1,
                   start = "random",
                   measure4diss = "EUCL",
                   method4agglo = "single",
                   seed = 42,
                   ...) {

  outputFunProc(R)
  set.seed(seed)

  if (procedure == "kmeans")
    res <- clust.kmeans(dat_wide, k, iter.max, nstart, algorithm, ...)
  if (procedure == "kmeanspp")
    res <- clust.kmeanspp(dat_wide, k, start, iter.max, nstart, algorithm, ...)
  if (procedure == "hclust")
    res <- clust.hclust(dat_wide, measure4diss, method4agglo, k, ...)

  ## No "Done!" needes, as outputs comes from other function
  return (res)
}
