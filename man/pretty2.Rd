\name{pretty2}
\alias{pretty2}
\title{truncated pretty breakpoints}
\description{\code{\link{pretty}} with no values outside of x range}
\usage{pretty2(x, n=5, force=FALSE, ...)}
\arguments{
  \item{x}{object with numeric values}
  \item{n}{desired number of values in \code{\link{pretty}}. DEFAULT: 5}
  \item{force}{Must output lenght equal n exactly?  DEFAULT: FALSE}
  \item{\dots}{all other arguments in \code{\link{pretty}}.}
}
\details{calculates \code{pretty(x)}, then removes the values that do not lie within \code{\link{range}(x)}.\cr
If force=TRUE, range(x) is reduced step by step in a while loop until the condition is met.
This is useful if you want exactly 2 labels on an \code{\link{axis}}.
In order not to get stuck, the outer values are taken if there are more than n values within range(x). }
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Aug 2014}
\seealso{\code{\link{pretty}} , \code{\link{logVals}} }
\examples{
k <- c(135, 155, 120, 105, 140, 130, 190, 110)
range(k)
pretty(k)
pretty2(k)

pretty(c(0.2, 0.9), n=2)
pretty2(c(0.2, 0.9), n=2)
pretty2(c(0.2, 0.9), n=2, force=TRUE)
}
\keyword{dplot}
