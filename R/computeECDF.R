#' @title Compute ECDF
#' @export
computeECDF <- function(dat) {
  n = length(dat)
  x = sort(dat)
  y = seq(1, n) / n

  ## Compute confidence intervals
  KSd <- function(n) {
    ifelse(n > 80,
           1.358/(sqrt(n) + 0.12 + 0.11/sqrt(n)),
           splinefun(c(1:9, 10, 15, 10 * 2:8),
                     c(0.975, 0.84189, 0.7076, 0.62394, 0.56328,
                       0.51926, 0.48342, 0.45427, 0.43001, 0.40925,
                       0.3376, 0.29408, 0.2417, 0.21012, 0.18841,
                       0.17231, 0.15975, 0.1496))(n))
  }

  D <- sfsmisc::KSd(n)
  ci_l <- pmin(y - D, 1)
  ci_u <- pmin(y + D, 1)

  res <-
    data.frame(
      x = x,
      y = y,
      ci_lower = ci_l,
      ci_upper = ci_u
    )

  return(res)
}
