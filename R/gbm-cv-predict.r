#' Predictions for CV fitted GBM models
#'
#' Calculate predictions from cross validated generalized boosting
#' model from gbm2.
#'
#' @param object a GBMCVFit object containing CV gbm models
#'
#' @param gbm_data_obj a GBMData object containing all of the data
#' used to fit a gbm model.
#'
#' @param best_iter_cv number of trees with the smallest cv error
#'
#' @param cv_all return a matrix of predictions for all observations
#' or just a vector of predictions for the excluded observations.
#'
#' @param \dots not currently used
#'
#' @author James Hickey
#'
#' @return a matrix of predictions for each cv fold.
#' @importFrom stats predict
#' @export
predict.GBMCVFit <- function(object, data, best_iter_cv,
                             cv_all=FALSE,...) {

  # Extract fold info
  cv_folds <- length(object)-1
  cv_group <- object[[1]]$cv_group

  if (cv_all) {
    result <- matrix(nrow=nrow(data), ncol=cv_folds)
    for (ind in seq_len(cv_folds)) {
      model <- object[[ind+1]]
      my_data  <- data[, model$variables$var_names, drop=FALSE]
      predictions <- predict(model, newdata=my_data, n.trees=best_iter_cv)
      result[,ind] <- predictions
    }
  } else {

    ## test cv_group and data match
    if (nrow(data) != length(cv_group))
      stop("mismatch between data and cv_group")

    result <- double(nrow(data))
    for (ind in seq_len(cv_folds)) {
      excluded <- cv_group == ind
      model <- object[[ind+1]]
      my_data  <- data[excluded, model$variables$var_names, drop=FALSE]
      predictions <- predict(model, newdata=my_data, n.trees=best_iter_cv)
      result[excluded] <- predictions
    }
  }

  return(result)
}
