library(testthat)
library(bis557)
context("Test the output of glm_new.")



test_that("You glm_new() function works with a constant step size.", {

  n <- 5000
  p <- 3
  beta <- c(-1, 0.2, 0.1)
  X <- cbind(1, matrix(rnorm(n * (p- 1)), ncol = p - 1))
  eta <- X %*% beta
  lambda <- exp(eta)
  y <- rpois(n, lambda = lambda)

  fit_linear_model <- glm_new(X, y, mu_fun = function(eta) exp(eta), var_fun = identity,
                              step = 0.0001, method = "constant")

  fit_lm <- glm(y ~ X[,-1], family = "poisson")

  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients,
                    tolerance = 0.1)
})

test_that("Your glm_new() function works with an adaptive step size.", {

  n <- 5000
  p <- 3
  beta <- c(-1, 0.2, 0.1)
  X <- cbind(1, matrix(rnorm(n * (p- 1)), ncol = p - 1))
  eta <- X %*% beta
  lambda <- exp(eta)
  y <- rpois(n, lambda = lambda)

  fit_linear_model <- glm_new(X, y, mu_fun = function(eta) exp(eta), var_fun = identity, step = 0.0001,
                              friction = 0.9, method = "momentum")

  fit_lm <- glm(y ~ X[,-1], family = "poisson")

  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients,
                    tolerance = 0.1)
})



