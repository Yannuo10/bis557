#' @title optimization() function
#' @description one way to select the best lambda forridgr regressions.
#' @param form a formula;
#' @param dataset a data frame used for the function;
#' @param contrasts a list of contrasts for factor variables
#' @param lambdas a sequence for selection
#' @param nfold number of folds to be used
#' @examples
#' library(rsample)
#' library(rsample)
#' library(purrr)
#' library(foreach)
#' library(Matrix)
#' library(glmnet)
#' library(tibble)
#' library(doParallel)
#' registerDoParallel(cores=2)
#' library(MASS)
#' data(iris)
#' fit_linear_model <- optimization(Sepal.Length ~ ., iris, contrasts = list(Species = "contr.sum"), lambda = seq(0, 2, 0.01))
#' @export


optimization <- function(form, dataset, contrasts = NULL, lambdas = seq(-2, 2, 0.01), nfold = 10){

  folds <- vfold_cv(dataset, v = nfold)
  yname <- as.character(form)[2]
  mse <- foreach(lambda = lambdas) %dopar% {
    foreach(fold = folds$splits, .combine = c) %do% {
      df_train <- analysis(fold)
      X_train <- model.matrix(form, df_train, contrasts)
      y_train <- matrix(df_train[,yname], ncol = 1)
      df_test <- assessment(fold)
      X_test <- model.matrix(form, df_test, contrasts)
      y_test <- matrix(df_test[,yname], ncol = 1)
      fit <- ridge(form, df_train, contrasts = contrasts, lambda = lambda)
      beta <- fit$coefficients
      mean((y_test - X_test %*% beta)^2)
    }
  }

 table <- tibble(mean = lapply(mse, mean),
                      lambda = lambdas)

 lambda_min = table$lambda[which.min(table$mean)]

  ret <- list(lambda = lambda_min)
  class(ret) <- "best_lambda"
  ret
}



