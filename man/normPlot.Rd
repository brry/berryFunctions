\name{normPlot}
\alias{normPlot}
\title{normal density plot}
\description{nice plot of normal density distribution}
\usage{normPlot(mean=0, sd=1, width=3, lines=TRUE, fill=rgb(0,0.3,0.8, 0.4),
cumulative=TRUE, las=1, 
main=paste("Normal density with\\nmean =", signif(mean,2), "and sd =", signif(sd,2)),
ylim=lim0(y), ylab="", xlab="", type="l", lty=1, col=par("fg"),
mar=c(2,3,3,3), keeppar=FALSE, ... )}
\arguments{
  \item{mean}{average value as in \code{\link{dnorm}}. DEFAULT: 0}
  \item{sd}{standard deviation. DEFAULT: 1}
  \item{width}{distance (in sd) from plot ends to mean. DEFAULT: 3}
  \item{lines}{Should vertical lines be plotted at mean +- n*sd? DEFAULT: TRUE}
  \item{fill}{color passed to \code{\link{polygon}}. DEFAULT: rgb(0,0.3,0.8, 0.4)}
  \item{cumulative}{Should cumulative density distribution be added? DEFAULT: TRUE}
  \item{las}{arguments passed to \code{\link{plot}}. DEFAULT: 1}
  \item{main}{main as in \code{\link{plot}}. DEFAULT: paste("Normal density with\\nmean =", mean, "and sd =", sd)}
  \item{ylim}{limit for the y axis. DEFAULT: lim0(y)}
  \item{ylab, xlab}{labels for the axes. DEFAULT: ""}
  \item{type, lty, col}{arguments passed to \code{\link{plot}} and \code{\link{lines}}.}
  \item{mar}{margins for plot passed to \code{\link{par}}. DEFAULT: c(2,3,3,3)}
  \item{keeppar}{should margin parameters be kept instead of being restored to previous value? DEFAULT: FALSE}
  \item{\dots}{further arguments passed to \code{\link{plot}} like lwd, xaxs, cex.axis, etc.}
}
\details{This function finds some nice defaults for very quickly plotting a normal distribution by just specifying mean and sd.}
\value{None. Used for plotting.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, July 2014}
\seealso{\code{\link{betaPlot}}, \code{\link{dnorm}}, \url{http://cran.r-project.org/package=denstrip}, \url{http://cran.r-project.org/web/views/Distributions.html} }
\examples{
normPlot()
normPlot(81.7, 11.45)
}
\keyword{hplot}
\keyword{distribution}
