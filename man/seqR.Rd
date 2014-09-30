\name{seqR}
\alias{seqR}
\title{Add a range argument to seq}
\description{sequence given by range or vector of values.}
\usage{seqR(range, from=1, to=1, ...)}
\arguments{
  \item{range}{vector with 2 values (1st taken as \code{from}, 2nd as \code{to}) or more (the result is then always ascending).}
  \item{from}{start value of sequence. DEFAULT:1}
  \item{to}{end value of sequence. DEFAULT:1}
  \item{\dots}{further arguments passed to \code{\link{seq}}.}
}
\value{Numeric vector.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2014}
\seealso{\code{\link{seq}}, \code{\link{range}}, \url{http://r.789695.n4.nabble.com/seq-range-argument-td4684627.html}}
\examples{
seqR(range=c(12,6), by=-2)

m <- c(41, 12, 38, 29, 50, 39, 22)
seqR(m, by=3)
# Takes min and max of range if the vector has more than two elements.

seqR(extendrange(m, f=0.1), len=5)
}
\keyword{datagen}
