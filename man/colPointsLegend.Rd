\name{colPointsLegend}
\alias{colPointsLegend}
\title{Legend for colPoints}
\description{Adds legends to plots created or enhanced with \code{\link{colPoints}}}
\usage{colPointsLegend(z, Range=range(z, finite=TRUE), nbins=40,
colors=rainbow2(nbins), bb=seqR(Range, length.out=nbins+1), at=pretty2(Range),
labels=at, bg="white", x1=60, y1=99, x2=x1+38, y2=y1-11, mar, mgp=c(1.8, 0.6, 0),
sborder=NA, resetfocus=TRUE, plottriangle=FALSE, triangle=0.14, tricol=c(1,8),
density=NULL, lines=TRUE, atminmax=FALSE,
horizontal=TRUE, labelpos=1, titlepos=3, title="Legend", las=1, ...)}
\arguments{
  \item{z}{Values of third dimension used in \code{\link{colPoints}}, can be matrix, vector, etc, but must be numeric}
  \item{Range}{Ends of color bar. DEFAULT: range(z, finite=TRUE)}
  \item{nbins}{Number of classes (thus, colors). DEFAULT: 40}
  \item{colors}{Color vector. DEFAULT: \code{\link{rainbow}} from blue (lowest) to red (highest value in Range)}
  \item{bb}{Borders of bins for the legend (key). DEFAULT: seqR(Range, length.out=nbins+1)}
  \item{at}{Positions of legend labels. DEFAULT: pretty2(Range)}
  \item{labels}{Labels that are written at the positions of \code{at}. DEFAULT: at}
  \item{bg}{Background behind key, labels and title. DEFAULT: "white"}
  \item{x1,y1}{Topleft relative coordinates (0:100) of inset plot, see \code{\link{smallPlot}}. DEFAULT: 60,99}
  \item{x2,y2}{Bottomright -"-. DEFAULT: 98,88}
  \item{mar}{Margins for \code{\link{smallPlot}} in relative values (0:100). DEFAULT: internal calculations based on title, labelpos and titlepos.}
  \item{mgp}{MarGinPlacement: distance of xlab/ylab, numbers and line from plot margin, as in \code{\link{par}}, but with different defaults. DEFAULT: c(1.8, 0.6, 0)}
  \item{sborder}{Border around inset subplot. DEFAULT: NA}
  \item{resetfocus}{Reset focus to original plot? Specifies where further low level plot commands are directed to. DEFAULT: TRUE}
  \item{plottriangle}{Should triangles be plotted at the end of the legend for values outside Range? TRUE if missing but triangle is given. DEFAULT: FALSE}
  \item{triangle}{Percentage of bar length at lower and upper end for triangles (can be a vector with two different values). DEFAULT: 0.14}
  \item{tricol}{Triangle colors for lower and upper end. DEFAULT: c(1,8)}
  \item{density}{Plot kernel density line? arguments passed to \code{\link{density}}. DEFAULT: NULL}
  \item{lines}{Plot black lines in the color bar at \code{at}? DEFAULT: TRUE}
  \item{atminmax}{Should the extrema of the legend be added to \code{at}? DEFAULT: FALSE}
  \item{horizontal}{Horizontal bar? if FALSE, a vertical bar is drawn. DEFAULT: TRUE}
  \item{labelpos}{Position of labels relative to the bar. Possible: 1 (below), 2 (left), 3 (above), 4 (right), 5(on top of bar). DEFAULT: 1}
  \item{titlepos}{Position of title -"-. DEFAULT: 3}
  \item{title}{Legend title. DEFAULT: "Legend"}
  \item{las}{LabelAxisStyle. DEFAULT: 1}
  \item{\dots}{Further arguments passed to \code{\link{text}} and \code{\link{strwidth}}, e.g. cex, srt, font, col. But NOT adj!}
}
\details{\code{x1,x2,y1,y2,labelpos,titlepos,title} have different defaults when \code{horizontal=FALSE}.}
\value{invisible list of par of smallPlot, adds legend bar to current plot}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2012-2014}
\seealso{\code{\link{colPoints}} (section examples) for real life example}
\examples{
z <- rnorm(50)
plot(1:10)
colPointsLegend(z=z)
colPointsLegend(z=z, titlepos=2)
colPointsLegend(z=z, horiz=FALSE) # note the different defaults
# positioning relative to plot:
colPointsLegend(z=z, x1=5, x2=30, y1=90, y2=70, title="Booh!", density=FALSE)
# Denote values outside of Range wit a triangle:
colPointsLegend(z=z, Range=c(-1,3), x1=20, y1=60, y2=40, triangle=c(0,0.5))
colPointsLegend(z=z, horiz=FALSE, x1=70, y1=60, plottriangle=TRUE, density=FALSE)
?colPoints # example section for actual usage
}
\keyword{aplot}
\keyword{color}
