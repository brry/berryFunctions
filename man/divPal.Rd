\name{divPal}
\alias{divPal}
\title{Diverging color palette}
\description{Diverging color palette: brown to blue, light colors in the middle, darker at the extremes}
\usage{divPal(n=12, reverse=FALSE, alpha=1, rwb=FALSE, colors=NULL, ... )}
\arguments{
  \item{n}{Number of colors. DEFAULT: 12}
  \item{reverse}{Reverse colors? DEFAULT: FALSE}
  \item{alpha}{Transparency (0=transparent, 1=fully colored). DEFAULT: 1}
  \item{rwb}{Should colors be in red-white-blue instead of brown-blue? DEFAULT: FALSE}
  \item{colors}{If not NULL, a color vector used in \code{\link{colorRampPalette}}. DEFAULT: NULL}
  \item{\dots}{Further arguments passed to \code{\link{colorRamp}}}
}
\value{Character string vector with color names}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Jan 2016}
\references{Originally in 12 shades in the IPCC Assesment Report 5 Chapter 12 Fig 12.22, \url{http://www.ipcc.ch/report/ar5/wg1/}}
\seealso{\code{\link{seqPal}}, \code{\link{addAlpha}}, \code{\link{colorRampPalette}}, package RColorBrewer}
\examples{
op <- par(mfrow=c(9,1), mar=c(0,0,0,0))
for(n in c(3,7,11,12,300)) plot(rep(1,n), pch=16, cex=5, col=divPal(n), xaxt="n")
plot(rep(1,12), pch=16, cex=5, col=divPal(alpha=0.4), xaxt="n")
plot(rep(1,12), pch=16, cex=5, col=divPal(rev=TRUE), xaxt="n")
plot(rep(1,12), pch=16, cex=5, col=divPal(rwb=TRUE), xaxt="n")
plot(rep(1,12), pch=16, cex=5, col=divPal(col=c("yellow","red")), xaxt="n")
par(op)

}
\keyword{color}
\keyword{dplot}
