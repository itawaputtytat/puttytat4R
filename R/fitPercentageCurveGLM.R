#' @title Smooth values using GLM
#' @export

fitPercentageCurveGLM <- function(dat,
                   col_name_reference,
                   col_name_value,
                   polypar = 4,
                   family = quote(gaussian(link = "log"))) {

  outputFunProc(R)

  ## Ajustment for avoiding overfitting at start and end of vector
  ## To explore computing steps use View(test) as overview of coll
  ## (test must be initiated in global environment at first)
  # coll <- list(dat)
  for (i in 1:nrow(dat)) {
    ## New data:
    ## First row to second last value
    ## New row: Column means of last two rows
    ## Last row: Actual last row
    dat <-
      rbind(dat[1:(nrow(dat)-1), ],
            colMeans(tail(dat, 2)),
            tail(dat, 1))

    # coll <- c(coll, list(dat))
  }
  # test <<- coll

  for (i in nrow(dat):1) {
    ## New data:
    ## First row to second last value
    ## New row: Column means of last two rows
    ## Last row: Actual last row
    dat <-
      rbind(dat[1, ],
            colMeans(head(dat, 2)),
            dat[2:nrow(dat), ])
    #coll <- c(coll, list(dat))
  }


  ## Create new data for prediction
  dat_reference <-
    data.frame(
      seq(min(dat[, col_name_reference]),
          max(dat[, col_name_reference]),
          0.1)
    )
  colnames(dat_reference) <- col_name_reference

  if(!is.na(polypar)) {

    ## Create formula
    formula_glm <-
      paste0(col_name_value, " ~ poly(", col_name_reference, ", ", polypar, ")")

    ## Compute model
    fit <- glm(as.formula(formula_glm),
               data = dat,
               family = eval(family))
    #family = Gamma(link = "log"))

    ## Predict new values
    pred <- predict(fit, dat_reference, type = "response")

    # ## Adjust curve (if last row prediction was > 100, than 100)
    for (element in 2:length(pred))
      if (pred[element - 1] >= 100 & pred[element] > 98) {
        pred[element] <- 100
      }

    ## Adjust curve (if next row prediction is < 2 and smaller than current)
    for (element in 1:(length(pred) - 1))
      if (pred[element] > pred[element + 1] & pred[element + 1] < 2) {
        pred[element + 1] <- pred[element]
      }

    ## Adjust curve (no values above 100 percent)
    pred[pred > 100] <- 100

    ## Round values to 2 decimals
    pred <- round(pred, 2)

  } else {
    pred <- rep(0, nrow(dat_reference))
  }

  ## Create final dataframe
  dat_reference <-
    dat_reference %>%
    mutate_(.dots = setNames(list(interp(~v, v = pred)),
                             col_name_value))

  return(dat_reference)
}
