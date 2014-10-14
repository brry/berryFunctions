\name{colPointsHist}
\alias{colPointsHist}
\title{Histogram for colPoints}
\description{Adds Histogram to plots created or enhanced with \code{\link{colPoints}}}
\usage{colPointsHist(z, nbins=40, colors=rainbow2(nbins),
bb=seqR(z, length.out=nbins+1), at=pretty2(z), labels=at, bg="white",
x1=0, y1=30, x2=40, y2=0, mar=c(6, 7, 3, 2), mgp=c(1.8, 0.6, 0),
sborder=NA, resetfocus=TRUE, breaks=20, freq=TRUE, col=par("fg"),
border=NA, main="", ylab="", xlab="", las=1, axes=TRUE, ...)}
\arguments{
  \item{z}{Values of third dimension used in \code{\link{colPoints}}}
  \item{nbins}{Number of classes (thus, colors). DEFAULT: 40}
  \item{colors}{Colors that are used for the background. DEFAULT: rainbow2(nbins)}
  \item{bb}{Borders of bins for the background. DEFAULT: seqR(z, length.out=nbins+1)}
  \item{at}{Positions of x-axis labels. DEFAULT: pretty2(z)}
  \item{labels}{X-axis labels themselves. DEFAULT: at}
  \item{bg}{Background behind background and axis labels. DEFAULT: "white"}
  \item{x1, y1}{Topleft relative coordinates (0:100) of inset plot, see \code{\link{smallPlot}}. DEFAULT: 0,30}
  \item{x2, y2}{Bottomright -"-. DEFAULT: 40, 0}
  \item{mar}{Margins for \code{\link{smallPlot}} in relative values (0:100). DEFAULT: c(6, 7, 3, 2)}
  \item{mgp}{MarGinPlacement: distance of xlab/ylab, numbers and line from plot margin, as in \code{\link{par}}, but with different defaults. DEFAULT: c(1.8, 0.6, 0)}
  \item{sborder}{Border around inset subplot. DEFAULT: par("fg")}
  \item{resetfocus}{Reset focus to original plot? Specifies where further low level plot commands are directed to.. DEFAULT: TRUE}
  \item{breaks}{Breaks as in \code{\link{hist}}, but with a different default. DEFAULT: 20}
  \item{freq}{Plot count data in hist? (if FALSE, plot density instead). DEFAULT: TRUE}
  \item{col}{Color of histogram bars. DEFAULT: par("fg")}
  \item{border}{Border around each bar. DEFAULT: NA}
  \item{main, ylab, xlab}{Labels. DEFAULT: ""}
  \item{las}{LabelAxisStyle. DEFAULT: 1}
  \item{axes}{Draw axes?. DEFAULT: TRUE}
  \item{\dots}{Further arguments passed to \code{\link{hist}}. NOT POSSIBLE: \code{x, add}}
}
\value{invisible list of par of smallPlot, adds legend bar to current plot}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2014}
\seealso{\code{\link{colPoints}} (section examples) for real life example}
\examples{
z <- rnorm(50)
plot(1:10)
colPointsHist(z=z)
}
\keyword{aplot}
\keyword{color}
