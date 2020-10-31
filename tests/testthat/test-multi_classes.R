library(testthat)
library(bis557)
context("Test the output of multi_class.")



test_that("You multi_class() function works with three classes.", {

  data(iris)
  #remove enough variables to avoid singularity (lead to warning messages otherwise)
  X <- cbind(1, as.matrix(iris[, -c(2, 3, 4, 5)]))
  y<-ifelse(iris$Species==levels(iris$Species)[1], 1, ifelse(iris$Species==levels(iris$Species)[2], 2, 3))

  fit_linear_model <- multi_class(X, y, itr = 50, tolerance = 1e-10)

  y1 <- ifelse(y==1, 1, 0)
  fit_lm1 <- glm(y1 ~ X[, -1], family = "binomial")


  y2 <- ifelse(y==2, 1, 0)
  fit_lm2 <- glm(y2 ~ X[, -1], family = "binomial")


  y3 <- ifelse(y==3, 1, 0)
  fit_lm3 <- glm(y3 ~ X[, -1], family = "binomial")


  expect_equivalent(fit_lm1$coefficients, fit_linear_model$coefficients[1, 1:2],
                    tolerance = 0.01)
  expect_equivalent(fit_lm2$coefficients, fit_linear_model$coefficients[2, 1:2],
                    tolerance = 0.01)
  expect_equivalent(fit_lm3$coefficients, fit_linear_model$coefficients[3, 1:2],
                    tolerance = 0.01)
})

