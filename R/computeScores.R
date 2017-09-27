#' @title Recode/Invert items
#' @description Recodes items from questionnaires
#' @param dat Data object
#' @param score_namess Names for target scores
#' @param list4items List of item/column names for each score in score_names
#' @param fun Set function for score computation as character: "mean" or "sum"
#' @export
computeScores <- function(dat, score_names, score_items_list, fun = "mean", compZ = F) {

  for(s in 1:length(score_names)) {

    score_name <- tolower(score_names[s])

    if (fun == "mean") {
      dat[, score_name] <- rowMeans(dat[, score_items_list])
    }

    if (fun == "sum")  {
      dat[, score_name] <- rowSums(dat[, score_items_list])
    }

    if (compZ) {
      score_name_z <- paste_(score_name, "z")
      dat[, score_name_z] <- scale(dat[, score_name], center = T, scale = T)
    }

  }

  return(dat)
}
