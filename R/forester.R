#' @param data data.frame, matrix, data.table or dgCMatrix - training data set to create model, if data_test = NULL, then data will be
#' automatically divided into training and testing dataset. NOTE: data has to contain the target column.
#' @param target character: name of the target column, should be character and has to be column name in data.
#' @param type character: defining the task. Two options are: "regression" and "classification", particularly, binary classification.
#' @param metric character, name of metric used for evaluating best model. For regression, options are: "mse", "rmse", "mad" and "r2".
#' For classification, options are: "auc", "recall", "precision", "f1" and "accuracy".
#' @param data_test optional argument, class of data.frame, matrix, data.table or dgCMatrix - test data set used for evaluating model performance.
#' @param train_ratio numeric, ranged from between 0 and 1, indicating the proportion of splitting data train over original dataset, the remained data as data test would be used for measuring model-performance.
#' @param fill_na logical, default is FALSE. If TRUE, missing values in target column are removed, missing values in categorical columns are replaced by mode and
#' missing values in numeric columns are substituted by median of corresponding columns.
#' @param num_features numeric, default is NULL. Parameter indicates number of most important features, which are chosen from the train dataset. Automatically, those important
#' features will be kept in the train and test datasets.
#' @param tune logical. If TRUE, function will perform the hyperparameter tuning steps for each model inside.
#' @param tune_iter number (default: 20) - total number of times the optimization step is to repeated. This argument is used when tune = TRUE.
#'
#'
#' @return An object of the class \code{forester_model} which is the best model with respect to the
#' chosen metric. It's also an object of the class \code{explainer} from DALEX family inherited the
#' explanation for the best chosen model.
#'
#' @export
#' @importFrom stats predict
#' @examples
#' \donttest{

forester <- function(data, target, type, metric = NULL, data_test = NULL, train_ratio = 0.8, fill_na = TRUE, num_features = NULL, tune = FALSE, tune_iter = 20, refclass = NULL){

  message("__________________________")
  message("FORESTER")
  data <- check_condition(datas, target, type="classification")

  ### If data_test is blank, it is needed to split data into data_train and data_test by the train_ratio
  if (is.null(data_test)){
    splited_data <- split_data(data, target, type)
    data_train <- splited_data[[1]]
    data_test <- splited_data[[2]]

  } else {
    data_test <- check_condition(data_test, target, type)
    data_train <- data

    # Check structure of data_test:
    if (!(setequal(colnames(data_train),colnames(data_test)))){
      stop("Column names in train data set and test data set are not identical.")
    }
  }

  data_for_messages <- prepare_data(data_train = data_train, target = target, type = type,
                                    fill_na = fill_na, num_features = num_features)

  message("__________________________")
  message("CREATING MODELS")

  ### Creating models, checking for the installed packages

  is_available_ranger <- try(
    suppressMessages(ranger_exp <- make_ranger(data = data_train, target = target, type = type,
                                               tune = tune, tune_metric = metric,
                                               tune_iter = tune_iter, fill_na = fill_na,
                                               num_features = num_features,
                                               refclass = refclass)),
    silent = TRUE
  )
  if (class(is_available_ranger) == "try-error") {
    ranger_exp <- NULL
    message("--- Omitting `make_ranger()` because the `ranger` package is not available ---")
  } else {
    message("--- Ranger model has been created ---")
  }

  is_available_catboost <- try(
    suppressMessages(catboost_exp <- make_catboost(data = data_train, target = target, type = type,
                                                   tune = tune, tune_metric = metric,
                                                   tune_iter = tune_iter, fill_na = fill_na,
                                                   num_features = num_features)),
    silent = TRUE
  )
  if (class(is_available_catboost) == "try-error") {
    catboost_exp <- NULL
    message("--- Omitting `make_catboost()` because the `catboost` package is not available ---")
  } else {
    message("--- Catboost model has been created ---")
  }

  is_available_xgboost <- try(
    suppressMessages(xgboost_exp  <- make_xgboost(data = data_train, target = target, type = type,
                                                  tune = tune, tune_metric = metric,
                                                  tune_iter = tune_iter, fill_na = fill_na,
                                                  num_features = num_features)),
    silent = TRUE
  )
  if (class(is_available_xgboost) == "try-error") {
    xgboost_exp <- NULL
    message("--- Omitting `make_xgboost()` because the `xgboost` package is not available ---")
  } else {
    message("--- Xgboost model has been created ---")
  }

  is_available_lightgbm <- try(
    suppressMessages(lightgbm_exp <- make_lightgbm(data = data_train, target = target, type = type,
                                                   tune = tune, tune_metric = metric,
                                                   tune_iter = tune_iter, fill_na = fill_na,
                                                   num_features = num_features)),
    silent = TRUE
  )
  if (class(is_available_lightgbm) == "try-error") {
    lightgbm_exp <- NULL
    message("--- Omitting `make_lightgbm()` because the `lightgbm` package is not available ---")
  } else {
    message("--- LightGBM model has been created ---")
  }

  message("__________________________")
  message("COMPARISON")

  result <- evaluate(
    catboost_exp,
    xgboost_exp,
    ranger_exp,
    lightgbm_exp,
    data_test = data_test,
    target = target,
    metric = metric
  )

  return(list(best_model = result$best_model,
              model1     = result$model1,
              model2     = result$model2,
              model3     = result$model3,
              model4     = result$model4,
              test_data  = data_test))
}
