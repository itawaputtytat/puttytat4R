#' @title Interpolate complete data.frame
#' @export

intrpldf <- function(dat,
                     colname4ref,
                     template_min = NULL,
                     template_max = NULL,
                     template_stepsize = 1,
                     colnames2excl,
                     showLog = F) {

  ## Create template
  if (is.null(template_min))
    template_min <- min(dat[, colname4ref], na.rm = T)
  if (is.null(template_max))
    template_max <- max(dat[, colname4ref], na.rm = T)
  template <- seq(template_min, template_max, template_stepsize)
  template <- data.frame(template)
  colnames(template) <- colname4ref

  ## Merge template and data
  dat[, colname4ref] <- as.character(dat[, colname4ref])
  template[, colname4ref] <- as.character(template[, colname4ref])
  dat <- left_join(template, dat)
  dat[, colname4ref] <- as.numeric(dat[, colname4ref])
  col_n <- ncol(dat)

  ## Identify rows with missing values using second column
  rows_w_dat <- which(!is.na(dat[, 2]))
  rows_w_dat_min <- min(rows_w_dat)
  rows_na <- which(is.na(dat[, 2]))

  ## In case of no preceding values take actual first value
  dat[1:( rows_w_dat_min - 1 ), 2:col_n] <- dat[rows_w_dat_min, 2:col_n]

  ## Evaluate each column
  dat <- lapply(dat, function(currentcol) {
    ## In case of numeric values call function for numeric interpolation
    if(is.numeric(currentcol))
      newvals <- intrplNum(currentcol, rows_w_dat) else
      #newvals <- na.approx(currentcol, na.rm = F) else
        ## Convert to character as workaround
        newvals <- as.character(na.locf(currentcol))
  } )
  dat <- as.data.frame(dat)

  ## LOG
  ## Interpolation necessary?

  ## Code interpolated values as T
  dat$intrpl <- F
  dat$intrpl[rows_na] <- T

  if(showLog) {
    cat("* Row numbers before:", length(rows_w_dat), "\n")
    cat("* Row numbers after: ", length(rows_w_dat) + length(rows_na), "\n")
  }

  return(dat)
}


