#' Predictions for CV fitted GBM models
#'
#' Calculate predictions from cross validated generalized boosting
#' model from gbm2.
#'
#' If cv_groups is NULL, the predictions from the models fitted to
#' each cv subset are returned as a matrix.  Otherwise this argument
#' should be an integer vector indicating which of the k-fold models
#' should be used to predict each observation.
#'
#' @param object a GBMCVFit object containing CV gbm models
#'
#' @param data a GBMData object containing all of the data
#' used to fit a gbm model.
#'
#' @param best_iter_cv number of trees with the smallest cv error
#'
#' @param cv_group NULL or an integer vector indicating which model
#' should be used fro prediction.
#'
#' @param \dots other options to \code{\link{predict.GBMFit}}
#'
#' @author James Hickey
#'
#' @return either a vector or matrix of predictions.
#' @importFrom stats predict
#' @export
predict.GBMCVFit <- function(object, data, best_iter_cv,
                             cv_group=NULL,...) {

  # Extract fold info
  cv_folds <- length(object)-1

  if (is.null(cv_group)) {
    result <- matrix(nrow=nrow(data), ncol=cv_folds)
    for (ind in seq_len(cv_folds)) {
      model <- object[[ind+1]]
      my_data  <- data[, model$variables$var_names, drop=FALSE]
      predictions <- predict(model, newdata=my_data, n.trees=best_iter_cv,...)
      result[,ind] <- predictions
    }
  } else {

    ## test cv_group and data match
    if (nrow(data) != length(cv_group))
      stop("mismatch between data and cv_group")

    result <- double(nrow(data))
    result[] <- NA
    for (ind in seq_len(cv_folds)) {
      excluded <- cv_group == ind
      model <- object[[ind+1]]
      my_data  <- data[excluded, model$variables$var_names, drop=FALSE]
      predictions <- predict(model, newdata=my_data, n.trees=best_iter_cv,...)
      result[excluded] <- predictions
    }
  }

  return(result)
}
