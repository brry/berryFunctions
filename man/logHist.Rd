\name{logHist}
\alias{logHist}
\title{Histogram of logarithmic values}
\description{Draw histogram of values on a logarithmic scale with nice axis labels}
\usage{logHist(x, logargs=NULL, main=xmain, xlab=xname, ...)}
\arguments{
  \item{x}{Vector of numerical values}
  \item{logargs}{A list of arguments passed to \code{\link{logAxis}}. DEFAULT: NULL}
  \item{main}{Title of graph, internally from x. DEFAULT: xmain}
  \item{xlab}{X axis label. DEFAULT: xname}
  \item{\dots}{further arguments passed to \code{\link{hist}} like breaks, col, ..., but not xaxt or add.}
}
\value{none}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Feb 2016}
\seealso{\code{\link{logAxis}}, \code{\link{hist}} }
\examples{
dat <- rbeta(1e4, 2, 18)*100
hist(dat, col="tan", breaks=50)
logHist(dat, col="tan", breaks=50)
}
\keyword{aplot}
\keyword{dplot}
