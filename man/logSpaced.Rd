\name{logSpaced}
\alias{logSpaced}
\title{Logarithmically spaced points}
\description{Calculates values that are in logarithmic distance from each other.}
\usage{logSpaced(base=1.2, n=20, min=1, max=n,
plot=TRUE, pch=3, las=1, ylab="base", ... )}
\arguments{
  \item{base}{Base for calculations, can be a vector to compare several bases. DEFAULT: 1.1}
  \item{n}{Number of values to be calculated. DEFAULT: 30}
  \item{min,max}{Range where n values are to be distributed, single values each. DEFAULT: 1,n}
  \item{plot}{Should the points be plotted on a line? DEFAULT: TRUE}
  \item{pch, las}{PointCharacter and Label Axis Style. DEFAULT: 3,1}
  \item{ylab}{Y axis label. DEFAULT: "base"}
  \item{\dots}{Further arguments passed to \code{\link{plot}}}
}
\value{Vector or matrix, depending on base input}
\note{base >1 concentrate points at low values, base<1 at high values.
The default values may change yet, so do specify them in your code.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Oct 2014}
\seealso{\code{\link{classify}}, \code{\link{log}} }
\examples{
logSpaced()
logSpaced(base=c(1.1, 1.5, 2), n=6, min=5, max=10)
d <- logSpaced(seq(0.8, 1.2, 0.025), main="logarithmically spaced points")

}
\keyword{arith}
