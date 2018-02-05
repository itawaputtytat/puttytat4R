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
  if (is.null(min)) {
    min <- min(dat[, colname4ref], na.rm = T)
    min <- round(min, getDecimalPlaces(stepsize))
  }

  if (is.null(max)) {
    max <- max(dat[, colname4ref], na.rm = T)
    max <- round(max, getDecimalPlaces(stepsize))
  }

  ## Convert to character for correct merging
  template <- seq(min, max, stepsize)
  template <- round(template, getDecimalPlaces(stepsize))
  template <- as.character(template, str)
  template <- data.frame(template)
  colnames(template) <- colname4ref

  ## Prepare data for merging
  ## For correct merging values have to be converted into character
  ## Also round values as workaround for correct merging
  dat[, colname4ref] <- round(dat[, colname4ref], getDecimalPlaces(stepsize))
  dat[, colname4ref] <- as.character(dat[, colname4ref])

  ## In case data frequency is higher than intended step size
  dat <-
    dat %>%
    group_by_(colname4ref) %>%
    summarise_all("max") %>%
    arrange_(colname4ref)

  ## Merge template and data
  dat <- left_join(template, dat, by = colname4ref)

  dat[, colname4ref] <- as.numeric(dat[, colname4ref])
  col_n <- ncol(dat)

  #print(head(dat, 15))

  ## Identify rows with missing values using first of non-reference-columns
  col_finder <- grep(colname4ref, colnames(dat), value = T, invert = T)[1]
  rows_w_dat <- which(!is.na(dat[, col_finder]))
  #rows_w_dat <- max(rows_w_dat, 1)
  rows_w_dat_min <- min(rows_w_dat)
  rows_na <- which(is.na(dat[, col_finder]))
  rows_na_preceding <- rows_na[rows_na < min(rows_w_dat)]

  colnames_backup <- colnames(dat)

  ## Necessary to avoid shortage of reference columns
  colnames2excl <- c(colnames2excl, colname4ref)

  ## Evaluate each column
  dat_new <- lapply(colnames(dat), function(currentcol) {

    ## In case of numeric values call function for numeric interpolation
    if (!currentcol %in% colnames2excl) {

      # In case of no preceding values take actual first value
      if (replace_preceding & length(rows_na_preceding) > 0) {
        dat[1:( rows_w_dat_min - 1 ), currentcol] <- dat[rows_w_dat_min, currentcol]
        ## THIS DOES NOT WORK
        #dat[, currentcol] <- zoo::na.locf(dat[, currentcol], fromLast = T, na.rm = F)
      }

      ## In case of numeric values do linear interpolationg
      if (is.numeric(dat[, currentcol]) & !currentcol %in% binary_vars) { ## .. but remember how many decimal places the value had before
        newvals <- zoo::na.approx(dat[, currentcol], na.rm = F)
        #print(paste("* Processing", currentcol, "as numeric."))
        ## In case of non-numeric values: Carry on last observation
      } else {
        ## Convert to character as workaround
        #newvals <- as.character(zoo::na.locf(dat[, currentcol]))
        newvals <- zoo::na.locf(dat[, currentcol], na.rm = F)
        #print(paste("* Processing", currentcol, "as character."))
      }

      ## In case of columns that should not be interpolated
      ## ... take over former values
    } else {
      newvals <- dat[, currentcol]
    }
  } )

  #dat_new <- as.data.frame(dat_new)
  dat_new <- as.data.frame(dat_new, stringsAsFactors = F)
  #print(head(dat_new, 15))

  #dat <- as.data.frame(dat, stringsAsFactors = F)
  colnames(dat_new) <- colnames_backup

  ## LOG
  ## Interpolation necessary?

  ## Code interpolated values as T
  ## Consider case of individual column name for interpolation indicator
  dat_new[, colname_intrpld] <- F
  dat_new[rows_na, colname_intrpld] <- T

  if (showLog) {
    cat("* Row numbers before:", length(rows_w_dat), "\n")
    cat("* Row numbers after: ", length(rows_w_dat) + length(rows_na), "\n")
  }

  outputDone()
  return(dat_new)
}
