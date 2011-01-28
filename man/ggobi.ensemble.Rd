\name{ggobi.ensemble}
\alias{ggobi.ensemble}
\title{Load model ensemble into GGobi with appropriate edge structure...}
\usage{ggobi.ensemble(data, ...)}

\description{
  Load model ensemble into GGobi with appropriate edge
  structure
}
\keyword{dynamic}
\keyword{regression}
\arguments{
  \item{data}{model ensemble object}
  \item{...}{other arguments ignored}
}
\examples{y <- swiss$Fertility
x <- swiss[, -1]
mods <- fitall(y, x, "lm")
\dontrun{
library(rggobi)
ggobi(mods, swiss)
}}
