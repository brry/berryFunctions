\name{approx2}
\alias{approx2}
\title{smart linear NA interpolation}
\description{As \code{\link{approx}}, approx2 fills NAs in a vector with linear interpolation,
but unlike \code{\link{approx}}, it can handle NAs at the ends of a vector
(takes the first/last value available for those).
Also, approx2 returns a vector only.}
\usage{approx2(x, fill=NULL)}
\arguments{
  \item{x}{vector with values}
  \item{fill}{Function to fill NAs at the start or end of the vector. See Details. DEFAULT: NULL}
  }
\details{The function fill is used to fill missing values at the ends of the vector.
It could be mean or median, for example, but must be a function that accepts \code{na.rm=TRUE} as an argument.
The default (NULL) means to use the first (or last) observation available.
}
\value{Vector with NAs replaced with interpolation (not a list, as in \code{\link{approx}}!)}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, July 2015}
\seealso{\code{\link{approx}}, zoo::na.locf, \code{\link{ciBand}} for  usage example}
\examples{
# approx2(c(NA,NA)) # yields an error
approx2(c(NA,NA, 6, 4, 8, 9, 3, 2, 1))
approx2(c( 3,NA, 6, 4, 8, 9, 3, 2, 1))
approx2(c( 3, 4, 6, 4, 8, 9,NA, 2,NA))

approx2(c(NA,NA, 6, 4, 8, 9, 3, 2, 1))
approx2(c(NA,NA, 6, 4, 8, 9, 3, 2, 1), fill=median)
approx2(c(NA,NA, 6, 4, 8, 9, 3, 2, 1), fill=mean)

approx2(c( 3, 4, 6, 4, 8, 9,NA, 2,NA))
approx2(c( 3, 4, 6, 4, 8, 9,NA, 2,NA), fill=median)
approx2(c( 3, 4, 6, 4, 8, 9,NA, 2,NA), fill=mean)

}
\keyword{arith}
\keyword{dplot}
