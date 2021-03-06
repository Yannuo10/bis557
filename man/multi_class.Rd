% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_class.R
\name{multi_class}
\alias{multi_class}
\title{multi_class() function}
\usage{
multi_class(X, y, itr, tolerance)
}
\arguments{
\item{X}{a data matrix used for the function}

\item{y}{a vector of response values}

\item{itr}{number of iteration}

\item{tolerance}{allowed tolerance for two residuals}
}
\description{
one way to implement a classification model generalizing logistic regression to accommodate more than two classes
}
\examples{
data(iris); X <- cbind(1, as.matrix(iris[, -5]))
y <- ifelse(iris$Species==levels(iris$Species)[1], 1, ifelse(iris$Species==levels(iris$Species)[2], 2, 3))
fit_linear_model <- multi_class(X, y, itr = 50, tolerance = 1e-10)
}
