#' @title Recode/Invert items
#' @description Recodes items from questionnaires
#' @param dat Data object
#' @param name4scores Names for target scores
#' @param list4items List of item/column names for each score in name4score
#' @param fun Set function for score computation as character: "mean" or "sum"
#' @export
computeScores <- function(dat, name4score, list4items, fun = "mean", compZ = F) {

  for(s in 1:length(name4score)) {

    dat[, name4score[s]] <- NULL

    if (fun == "mean") score <- rowMeans(dat[, list4items[[s]]], na.rm = T)
    if (fun == "sum") score <- rowSums(dat[, list4items[[s]]], na.rm = T)

    dat <- cbind(dat, score)
    names(dat)[ncol(dat)] <- name4score[s]
  }

  if (compZ) {
    for(s in 1:length(name4score)) {
      score.z <- scale(dat[, name4score[s]], center = T, scale = T)
      dat <- cbind(dat, score.z)
      names(dat)[ncol(dat)] <- paste0(name4score[s], ".z")
    }
  }

  return(dat)
}
