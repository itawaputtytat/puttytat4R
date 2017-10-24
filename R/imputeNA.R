#' @title Impute values using mice
#' @export

imputeNA <- function(dat,
                     col_names_excl,
                     m = 50,
                     seed = 42,
                     method = "pmm",
                     printFlag = F) {


  if (reportNA(dat, col_names_excl, return_bool_only = T)) {
    ## Initialise parameters (without) imputating
    imp_model_init = mice(dat, maxit = 0)

    ## Select only necessary variables for prediction
    imp_model_init$predictorMatrix[, col_names_excl] <- 0

    ## Find values
    imp_model <-
      mice(dat,
           m = m,
           seed = seed,
           method = method,
           predictorMatrix = imp_model_init$predictorMatrix,
           printFlag = printFlag)

    ## Impute values
    dat_imp <- mice::complete(imp_model)

    return(dat_imp)
  } else {
    outputString("* No imputation necessary")
  }
}
