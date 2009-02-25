\name{ggobi.ensemble}
\alias{ggobi.ensemble}
\title{Explore a model ensemble with GGobi}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Load model ensemble into GGobi with appropriate edge structure
}
\usage{ggobi.ensemble(data, original=NULL, ...)}
\arguments{
\item{data}{}
\item{original}{}
\item{...}{}
}

\details{}

\examples{y <- swiss$Fertility
x <- swiss[, -1]
mods <- fitall(y, x, lm)
\dontrun{
library(rggobi)
ggobi(mods, swiss)
}}
\keyword{dynamic}
\keyword{regression}
