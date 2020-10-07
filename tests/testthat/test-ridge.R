library(testthat)
library(bis557)
library(MASS)
context("Test the output of ridge.")



test_that("You ridge() function works in an easy case.", {

  data(iris)

  fit_linear_model <- ridge(Sepal.Length ~ ., iris, lambda = 0.1)

  fit_lm <- lm.ridge(Sepal.Length  ~ ., iris, lambda = 0.1)

  expect_equivalent(coef(fit_lm), fit_linear_model$coefficients,
                    tolerance = 0.2)
})

test_that("Your ridge() function works with contrasts.", {

  data(iris)

  fit_linear_model <- ridge(Sepal.Length ~ ., iris,
                            contrasts = list(Species = "contr.sum"), lambda = 0.05)

  fit_lm <- lm.ridge(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"), lambda = 0.05)

  expect_equivalent(coef(fit_lm), fit_linear_model$coefficients,
                    tolerance = 0.2)
})
