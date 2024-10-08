% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_enet.R
\name{.get_enet}
\alias{.get_enet}
\title{Get penalized weights with the elastic net}
\usage{
.get_enet(x, y, lambda, nonzero)
}
\arguments{
\item{x}{
matrix of predictors
}

\item{y}{
response 
}

\item{lambda}{
Quadratic penalty parameter. lambda=0 performs the Lasso fit.
}

\item{nonzero}{x}
}
\value{
A matrix of penalized weights
}
\description{
This is an internal function.
}
\examples{
testing
}
