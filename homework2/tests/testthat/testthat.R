library(testthat)
library(homework2)
library(glmnet)
library(MASS)

context("Test the output of homework 2.")

test_that("Find coefficients using ridge regression", {

  data(iris)
  lambda = 0.01
  #x <- model.matrix(Sepal.Length~., iris)
  #y <- iris$Sepal.Length
  fit_ridge_model <- ridge_regression(Sepal.Length ~ ., iris)
  fit <- lm.ridge(Sepal.Length~., iris, alpha = 0, lambda = lambda)
  expect_equivalent(matrix(coef(fit)), matrix(fit_ridge_model$coefficients),
                    tolerance = 0.01)
})

test_that("Find the optimal lambda for ridge regression", {

  data(iris)
  #lambda = seq(0, nrow(data)*2, length.out = 500)
  #x <- model.matrix(Sepal.Length~., iris)
  #y <- iris$Sepal.Length
  lambda = seq(0, nrow(iris)*2, length.out = 500)
  #cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambda)
  ridge <- lm.ridge(Sepal.Length~., iris, alpha = 0, lambda = lambda)
  selected_lambda <- ridge_regression_lambda_selection(Sepal.Length ~ ., iris)
  expect_equivalent(ridge$lambda[which.min(ridge$GCV)], selected_lambda,
                    tolerance = 1e-5)
})


