\name{quantileMean}
\alias{quantileMean}
\title{Average of R's quantile methods}
\description{weighted average of \R's quantile methods}
\usage{quantileMean(x, probs = seq(0, 1, 0.25), weights=rep(1,9), names=TRUE, ... )}
\arguments{
  \item{x}{Numeric vector whose sample quantiles are wanted}
  \item{probs }{Numeric vector of probabilities with values in [0,1]. DEFAULT:  seq(0, 1, 0.25)}
  \item{weights}{Numeric vetor of length 9 with weight for each \code{\link{quantile} method}. Recycled if shorter. DEFAULT: unweighted mean. DEFAULT: rep(1,9)}
  \item{names}{If TRUE, the result has a names attribute. Set to FALSE for speedup with many probs.. DEFAULT: TRUE}
  \item{\dots}{further arguments passed to \code{\link{quantile}}, except for type}
}
\details{weights are internally normalized to sum 1}
\value{numeric named vector, as returned by \code{\link{apply}}}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Sept 2014}
\seealso{\code{\link{quantile}} }
\examples{
exDat <- rnorm(30,sd=5)
quantile(exDat, probs=c(0.9, 0.99), type=1)
quantile(exDat, probs=c(0.9, 0.99), type=2)
round( sapply(1:9, function(m) quantile(exDat, probs=0.9, type=m)) , 3)
# and now the unweighted average:
quantileMean(exDat, probs=c(0.9, 0.99))
quantileMean(exDat, probs=0.9)
# say I trust type 2 and 3 especially and want to add a touch of 7:
quantileMean(exDat, probs=c(0.9, 0.99), weights=c(1,5,5,0,1,1,3,1,1))
}
\keyword{univar}
