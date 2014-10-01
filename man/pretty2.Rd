\name{pretty2}
\alias{pretty2}
\title{truncated pretty breakpoints}
\description{\code{\link{pretty}} with no values outside of x range}
\usage{pretty2(x, ...)}
\arguments{
  \item{x}{object with numeric values}
  \item{\dots}{all other arguments in \code{\link{pretty}}}
}
\details{calculates \code{pretty(x)}, then removes the values that do not lie within \code{range(x)}. }
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Aug 2014}
\seealso{\code{\link{pretty}} , \code{\link{logVals}} }
\examples{
k <- c(135, 155, 120, 105, 140, 130, 190, 110)
range(k)
pretty(k)
pretty2(k)
}
\keyword{dplot}
