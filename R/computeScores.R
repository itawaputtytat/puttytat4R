#' @title Recode/Invert items
#' @description Recodes items from questionnaires
#' @param dat Data object
#' @param subscale_namess Names for target scores
#' @param list4items List of item/column names for each score in subscale_names
#' @param fun Set function for score computation as character: "mean" or "sum"
#' @export
computeScores <- function(dat,
                          subscale_items_list,
                          col_name_prefix = NULL,
                          col_name_prefix_overall = NULL,
                          lower_subscale_names = T,
                          fun = "mean",
                          compute_z = F,
                          compute_overall = T) {


  score_names <- names(subscale_items_list)
  if (!is.null(col_name_prefix)) {
    score_names <- paste(col_name_prefix, score_names, sep = "_")
  }

  score_name_overall <- "overall"
  if (!is.null(col_name_prefix_overall)) {
    score_name_overall <- paste_(col_name_prefix_overall, score_name_overall)
  }

  if (lower_subscale_names) {
    score_names <- tolower(score_names)
    score_name_overall <- tolower(score_name_overall)
  }

  for(s in 1:length(subscale_items_list)) {

    score_name_temp <- score_names[s]
    subscales_items_temp <- subscale_items_list[[s]]

    if (fun == "mean") {
      dat[, score_name_temp] <- rowMeans(dat[, subscales_items_temp])
    }

    if (fun == "sum")  {
      dat[, score_name_temp] <- rowSums(dat[, subscales_items_temp])
    }

    if (compute_z) {
      score_name_temp_z <- paste_(score_name_temp, "z")
      dat[, score_name_temp_z] <-
        scale(dat[, score_name_temp],
              center = T,
              scale = T)
    }

  } ## End of for loop

  if (compute_overall) {

    if (fun == "mean") {
      dat[, score_name_overall] <- rowMeans(dat[, subscales_items_temp])
    }

    if (fun == "sum") {
      dat[, score_name_overall] <- rowMeans(dat[, subscales_items_temp])
    }

    if (compute_z) {
      score_name_overall_z <- paste_(score_name_overall, "z")
      dat[, score_name_overall_z] <-
        scale(dat[, score_name_overall],
              center = T,
              scale = T)
    }
  }

  return(dat)
}
