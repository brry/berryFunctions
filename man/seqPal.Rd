\name{seqPal}
\alias{seqPal}
\title{Sequential color palette}
\description{Sequential color palette from yelow to red or yellow to blue or custom colors.}
\usage{seqPal(n=12, reverse=FALSE, alpha=1, yb=FALSE, colors=NULL, ... )}
\arguments{
  \item{n}{Number of colors. DEFAULT: 12}
  \item{reverse}{Reverse colors? DEFAULT: FALSE}
  \item{alpha}{Transparency (0=transparent, 1=fully colored). DEFAULT: 1}
  \item{yb}{Should colors be in yellow-blue instead of yellow-red? DEFAULT: FALSE}
  \item{colors}{If not NULL, a color vector used in \code{\link{colorRampPalette}}. DEFAULT: NULL}
  \item{\dots}{Further arguments passed to \code{\link{colorRamp}}}
}
\value{Character string vector with color names}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Jan 2016}
\seealso{\code{\link{divPal}}, \code{\link{addAlpha}}, \code{\link{colorRampPalette}}, package RColorBrewer}
\examples{
op <- par(mfrow=c(9,1), mar=c(0,0,0,0))
for(n in c(3,7,11,12,300)) plot(rep(1,n), pch=16, cex=5, col=seqPal(n), xaxt="n")
plot(rep(1,12), pch=16, cex=5, col=seqPal(alpha=0.4), xaxt="n")
plot(rep(1,12), pch=16, cex=5, col=seqPal(rev=TRUE), xaxt="n")
plot(rep(1,300), pch=16, cex=5, col=seqPal(300,yb=TRUE), xaxt="n")
plot(rep(1,300), pch=16, cex=5, col=seqPal(300,col=c("darkblue","green","orange")), xaxt="n")
par(op)

}
\keyword{color}
\keyword{dplot}
