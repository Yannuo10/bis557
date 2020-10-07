
library(testthat)
library(bis557)
context("Test output of the new grad_descent function.")



test_that("You gd_new() function works in an easy case.", {

  data(iris)

  fit_linear_model <- gd_new(Sepal.Length ~ ., iris)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients,
                    tolerance = 0.5)
})

test_that("Your gd_new() function works with contrasts.", {

  data(iris)

  fit_linear_model <- gd_new(Sepal.Length ~ ., iris,
                                   contrasts = list(Species = "contr.sum"))

  fit_lm <- lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))

  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients,
                    tolerance = 0.5)
})







