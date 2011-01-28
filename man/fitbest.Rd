\name{fitbest}
\alias{fitbest}
\title{Use the leaps package to generate the best subsets.}
\usage{fitbest(formula, data, nbest=10, ...)}

\description{
  Use the leaps package to generate the best subsets.
}
\keyword{regression}
\arguments{
  \item{formula}{model formula}
  \item{data}{data frame}
  \item{nbest}{number of subsets of each size to record}
  \item{...}{other arguments passed to \code{\link[leaps]{regsubsets}}}
}
\examples{y <- swiss$Fertility
mods <- fitbest(Fertility ~ ., swiss)}
