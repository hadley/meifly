\name{fitall}
\alias{fitall}
\title{Fit all combinations of x variables ($2^p$).}
\usage{fitall(y, x, method="lm", ...)}

\description{
  Fit all combinations of x variables ($2^p$).
}

\details{
  This technique generalises \code{\link{fitbest}}.  While
  it is much slower it will work for any type of model.
}
\keyword{regression}
\arguments{
  \item{y}{vector y values}
  \item{x}{matrix of x values}
  \item{method}{name of method used to fit the model, e.g
\code{\link{lm}},\code{\link[MASS]{rlm}}}
  \item{...}{other arguments passed on to \code{method}}
}
\examples{y <- swiss$Fertility
x <- swiss[, -1]
mods <- fitall(y, x, "lm")}
