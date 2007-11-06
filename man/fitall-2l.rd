\name{fitall}
\alias{fitall}
\title{Generate all models}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Fit all combinations of x variables ($2^p$)
}
\usage{fitall(y, x, method=lm, ...)}
\arguments{
\item{y}{vector y values}
\item{x}{matrix of x values}
\item{method}{method used to fit the model, eg \code{\link{lm}},\code{\link[MASS]{rlm}}}
\item{...}{}
}

\details{This technique generalises \code{\link{fitbest}}.  While it is much
slower it will work for any type of model.}

\examples{}
\keyword{regression}
