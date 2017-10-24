#' @title Report NA values
#' @export

reportNA <- function(dat, id_names, return_bool_only = F) {
  na_coll <- c()
  col_finder <- !grepl("subject_id", colnames(dat))
  col_finder <- colnames(dat)[col_finder]
  for (i in col_finder) {
    na_finder <- which(is.na(dat[, i]))
    if (length(na_finder) > 0) {
      na_coll[[i]] <- data.frame(dat[na_finder, id_names])
      colnames(na_coll[[i]]) <- id_names
    }
  }

  ## Output full report
  if (!return_bool_only) {
    if (length(na_coll) > 0) {
      outputWithSepLine("Missing values found in:")
      print(na_coll)
    } else {
      outputWithSepLine("No missing values found")
    }
  } else {
    ## Return TRUE if NA values were found
    return(length(na_coll) > 0)
  }
}
