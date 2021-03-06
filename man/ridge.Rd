% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ridge.R
\name{ridge}
\alias{ridge}
\title{ridge() function}
\usage{
ridge(form, df, contrasts = NULL, lambda = 0)
}
\arguments{
\item{form}{a formula;}

\item{df}{a data frame used for the function;}

\item{contrasts}{a list of contrasts for factor variables}

\item{lambda}{a constant for penalty}
}
\description{
another way to build a linear model. ridge regression for ordinary least squares
}
\examples{
data(iris)
fit_linear_model <- ridge(Sepal.Length ~ ., iris, contrasts = list(Species = "contr.sum"), lambda = 0.1)
}
