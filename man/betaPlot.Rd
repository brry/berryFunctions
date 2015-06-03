\name{betaPlot}
\alias{betaPlot}
\title{beta density plot}
\description{nice plot of beta density distribution}
\usage{betaPlot(shape1=1.5, shape2=5, lines=NA, fill=rgb(0,0.3,0.8, 0.4),
cumulative=TRUE, mar=c(2,3,3,3), keeppar=FALSE, las=1,
main=paste("Beta density with\\nalpha =", signif(shape1,2),
           "and beta =", signif(shape2,2)),
ylim=lim0(y), xlim=0:1, ylab="", xlab="",
type="l", lty=1, col=par("fg"), ... )}
\arguments{
  \item{shape1}{alpha value as in \code{\link{dbeta}}. DEFAULT: 1.5}
  \item{shape2}{beta value. DEFAULT: 5}
  \item{lines}{at which quantiles should vertical lines be plotted? DEFAULT: NA}
  \item{fill}{color passed to \code{\link{polygon}}. DEFAULT: rgb(0,0.3,0.8, 0.4)}
  \item{cumulative}{should cumulative density distribution be added? DEFAULT: TRUE}
  \item{mar}{margins for plot passed to \code{\link{par}}. DEFAULT: c(2,3,3,3)}
  \item{keeppar}{should margin parameters be kept instead of being restored to previous value? DEFAULT: FALSE}
  \item{las}{arguments passed to \code{\link{plot}}. DEFAULT: 1}
  \item{main}{main as in \code{\link{plot}}. DEFAULT: paste("Beta density with\\nalpha =", shape1, "and beta =", shape2)}
  \item{ylim, xlim}{limit for the y and x axis. DEFAULT: lim0(y), 0:1}
  \item{ylab, xlab}{labels for the axes. DEFAULT: ""}
  \item{type, lty, col}{arguments passed to \code{\link{plot}} and \code{\link{lines}}.}
  \item{\dots}{further arguments passed to \code{\link{plot}} like lwd, xaxs, cex.axis, etc.}
}

\details{This function very quickly plots a beta distribution by just specifying alpha and beta.}
\value{None. Used for plotting.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, July 2014}
\seealso{\code{\link{normPlot}}, \code{\link{dbeta}}, \url{http://cran.r-project.org/package=denstrip}, \url{http://cran.r-project.org/web/views/Distributions.html} }
\examples{


betaPlot()
betaPlot(2,1) 
betaPlot(0.5, 2)

# overview of parameters beta distribution:
# a bigger: HDI (Highest Density Interval) more to the left
# b bigger: HDI left, lower values
# both bigger: narrower HDI
a <- b <- c(0.5, 1:4)
par(mfrow=c(length(b), length(a)), yaxt="n", xaxt="n", 
    oma=c(2,2,4.5,2), mar=rep(0,4), mgp=c(3,0.7,0))
for(y in b)
  for(x in a)
  {
  betaPlot(x,y, main="", ylim=c(0.12,3), mar=rep(0,4), xaxs="r")
  mtext(text=paste("a=",x,", b=",y, sep=""), side=3, line=-1.5, cex=0.7)
  if(y==min(b)) mtext(paste("a =",x), side=3, line=0.5)
  if(x==min(a)) mtext(paste("b =",y), side=2, line=0.5)
  if(y==max(b) & x==median(a)) axis(1, at=c(0,0.5,1), xaxt="s", las=1)
  if(x==max(a) & y==median(b)) axis(4, at=0:3, yaxt="s", las=1)
  }
mtext("beta density distribution", line=2.5, outer=TRUE, cex=1.2, font=2)
box("outer")

}
\keyword{hplot}
\keyword{distribution}
