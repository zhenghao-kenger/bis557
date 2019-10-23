#' Perform ridge regression
#'
#' @param form A formilar.
#' @param data A dataset.
#' @param lambda A tuning parameter.
#' @return Construct a ridge regression model containing calculated coefficients.
#' @examples
#' ridge_regression(Sepal.Length~., iris)

ridge_regression <- function(form, data, lambda = 0) {
  rownames(data) <- NULL
  X <- model.matrix(form, data)
  Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]
  beta <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y
  attributes(beta)$formula <- form
  class(beta) <- c(class(beta), "ridge_regression")
  model <- list()
  model$coefficients <- beta
  model
}


#' Perform ridge regression and select the optimal lambda
#'
#' @param form A formilar.
#' @param data A dataset.
#' @param lambda A tuning parameter.
#' @return Construct a ridge regression model containing calculated coefficients and use mse to find the optimal lambda.
#' @examples
#' ridge_regression_lambda_selection(Sepal.Length~., iris)


ridge_regression_lambda_selection <- function(form, data, lambda = seq(0, nrow(data)*2, length.out = 500)){


lambda_selection <- function(form, data) {
  rownames(data) <- NULL
  lambda = seq(0, nrow(data)*2, length.out = 500)
  X <- model.matrix(form, data)
  Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]
  beta <- matrix(NA_real_, nrow = length(lambda), ncol = ncol(X))
  for (i in 1:length(lambda)){
    beta[i,] <- solve(crossprod(X) + diag(rep(lambda[i], ncol(X))) ) %*% t(X) %*% Y
  }
  beta
}

beta_mat <- lambda_selection(form, data)

folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
for(i in 1:10){
  ind <- which(folds==i,arr.ind=TRUE)
  test <- data[ind, ]
  train <- data[-ind, ]

  train_X <- model.matrix(form, train)
  train_Y <- train[all.vars(form)[1]]

  #test_X <- model.matrix(form, test)
  #test_Y <- test[all.vars(form)[1]]

  y_hat <- tcrossprod(train_X, beta_mat)
  mse <- apply((y_hat - train_Y)^2, 2, mean)
}
  selected_lambda <- lambda[which.min(mse)]
  return(selected_lambda)
}
