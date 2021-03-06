% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rescale.R
\name{rescale}
\alias{rescale}
\title{shift and scale a vector}
\usage{
rescale(x, from = 0, to = 1)
}
\arguments{
\item{x}{Numerical vector of values to be mapped to a given range}

\item{from}{output minimum. DEFAULT: 0}

\item{to}{output maximum. DEFAULT: 1}
}
\value{
numeric vector, rescaled onto output range
}
\description{
rescale a numeric vector: map values linearly onto a given range
}
\examples{

rescale(10:15, 135, 200)
rescale(10:15, 200, 135)
rescale(10:15, to=c(1,5))

values <- rbeta(1e3, shape1=4, shape2=35)
hist(rescale(values, 135, 200), breaks=25, col=3)

}
\references{
\url{https://stackoverflow.com/a/18303620}
}
\seealso{
\code{scales::rescale}
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de}, Jan 2016
}
\keyword{manip}
