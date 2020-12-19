library(testthat)
context("Test the output of grad_descent.")



test_that("You grad_descent() function works in an easy case.", {

  data(iris)

  fit_linear_model <- grad_descent(Sepal.Length ~ ., iris)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients,
                    tolerance = 1e-5)
})

test_that("Your grad_descent() function works with contrasts.", {

  data(iris)

  fit_linear_model <- grad_descent(Sepal.Length ~ ., iris,
                                      contrasts = list(Species = "contr.sum"))

  fit_lm <- lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))

  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients,
                    tolerance = 1e-5)
})




