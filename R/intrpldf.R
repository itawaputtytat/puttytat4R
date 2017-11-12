#' @title Interpolate complete data.frame
#' @export

intrpldf <- function(dat,
                      colname4ref,
                      min = NULL,
                      max = NULL,
                      stepsize = 1,
                      colnames2excl = NULL,
                      binary_vars = NULL,
                      showLog = F,
                      colname_intrpld = "intrpld",
                      replace_preceding = T) {

  outputFunProc(R)

  ## Create template
  if (is.null(min))
    min <- min(dat[, colname4ref], na.rm = T)
  if (is.null(max))
    max <- max(dat[, colname4ref], na.rm = T)
  template <- seq(min, max, stepsize)
  template <- data.frame(template)
  colnames(template) <- colname4ref

  ## Merge template and data
  ## For correct merging values have to be converted into character
  ## Also round values as workaround for correct merging
  dat[, colname4ref] <- round(dat[, colname4ref], getDecimalPlaces(stepsize))
  dat[, colname4ref] <- as.character(dat[, colname4ref])
  template <- round(template, getDecimalPlaces(stepsize))
  template[, colname4ref] <- as.character(template[, colname4ref])
  dat <- dplyr::left_join(template, dat, by = colname4ref)
  dat[, colname4ref] <- as.numeric(dat[, colname4ref])
  col_n <- ncol(dat)

  ## Identify rows with missing values using second column
  rows_w_dat <- which(!is.na(dat[, 2]))
  rows_w_dat_min <- min(rows_w_dat)
  rows_na <- which(is.na(dat[, 2]))

  colnames_backup <- colnames(dat)

  ## Necessary to avoid shortage of reference columns
  colnames2excl <- c(colnames2excl, colname4ref)

  ## Evaluate each column
  dat_new <- lapply(colnames(dat), function(currentcol) {

    ## In case of numeric values call function for numeric interpolation
    if (!currentcol %in% colnames2excl) {

      # In case of no preceding values take actual first value
      if (replace_preceding) {
        dat[1:( rows_w_dat_min - 1 ), currentcol] <- dat[rows_w_dat_min, currentcol]
        ## THIS DOES NOT WORK
        #dat[, currentcol] <- zoo::na.locf(dat[, currentcol], fromLast = T, na.rm = F)
      }

      ## In case of numeric values do linear interpolationg
      if (is.numeric(dat[, currentcol]) & !currentcol %in% binary_vars) { ## .. but remember how many decimal places the value had before
        newvals <- zoo::na.approx(dat[, currentcol], na.rm = F)

        ## In case of non-numeric values: Carry on last observation
      } else {
        ## Convert to character as workaround
        #newvals <- as.character(zoo::na.locf(dat[, currentcol]))
        newvals <- zoo::na.locf(dat[, currentcol], na.rm = F)
      }

      ## In case of columns that should not be interpolated
      ## ... take over former values
    } else {
      newvals <- dat[, currentcol]
    }
  } )

  dat_new <- data.frame(dat_new)
  #dat <- as.data.frame(dat, stringsAsFactors = F)
  colnames(dat_new) <- colnames_backup

  ## LOG
  ## Interpolation necessary?

  ## Code interpolated values as T
  dat$intrpld <- F
  dat$intrpld[rows_na] <- T

  ## In case of individual column name for interpolation indicator
  colnames(dat_new)[ncol(dat_new)] <- colname_intrpld


  if (showLog) {
    cat("* Row numbers before:", length(rows_w_dat), "\n")
    cat("* Row numbers after: ", length(rows_w_dat) + length(rows_na), "\n")
  }

  outputDone()
  return(dat_new)
}
