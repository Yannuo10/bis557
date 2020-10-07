library(testthat)
library(bis557)
library(rsample)
library(purrr)
library(foreach)
library(glmnet)
library(tibble)
library(doParallel)
library(MASS)


context("Test the output of optimization for lambda.")



test_that("You optimization() function works in an easy case.", {

  data(iris)

  fit_lm <- cv.glmnet(model.matrix(Sepal.Length  ~ ., iris), as.matrix(iris[, 1]),  alpha = 0)

  fit_linear_model <- optimization(Sepal.Length ~ ., iris, lambdas = fit_lm$lambda)


  expect_equivalent(fit_lm$lambda.min, fit_linear_model$lambda,
                    tolerance = 0.1)
})

test_that("Your optimization() function works with contrasts.", {

  data(iris)

  fit_lm <- cv.glmnet(model.matrix(Sepal.Length  ~ ., iris), as.matrix(iris[, 1]),
                      contrasts = list(Species = "contr.sum"),  alpha = 0)

  fit_linear_model <- optimization(Sepal.Length ~ ., iris,
                            contrasts = list(Species = "contr.sum"), lambdas = fit_lm$lambda)

  expect_equivalent(fit_lm$lambda.min, fit_linear_model$lambda,
                    tolerance = 0.1)
})
