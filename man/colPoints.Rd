\name{colPoints}
\alias{colPoints}
\title{add points to plot with color relative to third dimension data}
\description{
Draw colored points for 3D-data in a 2D-plane. Color is relative to third dimension, currently only on a linear scale.
Can take 3 vectors or, as in \code{\link{image}}, 2 vectors and a matrix for z.}
\usage{colPoints(x, y, z, Range=range(z, finite=TRUE), method="equalinterval", breaks,
sdlab=1, col=rev(rainbow(cl$nbins, start=0, end=0.7, alpha=1)), col2=par("fg"),
legargs=NULL, histargs=FALSE, add=TRUE, xlab=substitute(x), ylab=substitute(y),
las=1, pch=16, ...)}
\arguments{
  \item{x,y}{Vectors with coordinates of the points to be drawn}
  \item{z}{z values beloning to coordinates. Vector or matrix}
  \item{Range}{Ends of color bar for method=equalinterval. DEFAULT: range(z, finite=TRUE)}
  \item{method}{Classification method (partial matching is performed), see \code{\link{classify}}. DEFAULT: "equalinterval")}
  \item{breaks}{Specification for method, see \code{\link{classify}}. DEFAULT: different defaults for each method}
  \item{sdlab}{Type of label and breakpoints if \code{method=standarddeviation}, see \code{\link{classify}}. DEFAULT: 1}
  \item{col}{Vector of colors to be used. DEFAULT: \code{\link{rainbow}} from blue (lowest) to red (highest value in Range)}
  \item{col2}{Color for points not in the color range. DEFAULT: par("fg")}
  \item{legargs}{List. Arguments passed to \code{\link{colPointsLegend}}. \code{legargs=FALSE} to suppress drawing the legend, NULL for defaults. DEFAULT: NULL, with some defaults specified internally}
  \item{histargs}{List. Arguments passed to \code{\link{colPointsHist}}. -"-. DEFAULT: FALSE}
  \item{add}{Logical. Should the points be added to current (existing!) plot? If FALSE, a new plot is started. DEFAULT: TRUE (It's called col\bold{Points}, after all)}
  \item{xlab}{x-axis label. DEFAULT: substitute as in plot}
  \item{ylab}{y-axis label. DEFAULT: ditto}
  \item{las}{Label Axis Style. Only used when add=FALSE. See \code{\link{par}}. DEFAULT: 1 (all labels horizontal)}
  \item{pch}{Point CHaracter. See \code{\link{par}}. DEFAULT: 16}
  \item{\dots}{Further graphical arguments, eg cex, xlim (when add=F), mgp, main, sub, asp (when add=F), etc. Note: col does not work, as it is already another argument}
}
\value{Invisible list of values that can be passed to colPointsLegend or colPointsHist.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2011-2014. I'd be interested in hearing what you used the function for.}
\references{\url{http://uxblog.idvsolutions.com/2011/10/telling-truth.html}}
\seealso{\code{\link{classify}}, \code{\link{colPointsLegend}}, \code{\link{colPointsHist}} }
\examples{
i <- c( 22,  40,  80,  45,  60,  63,  30,  70,  55,  48,  32,  48,  70,  40)
j <- c(  5,  33,  12,  56,  20,  40,  45,  45,  30,  36,  23,  15,  30,  10)
k <- c(175, 174, 120, 105, 132, 130, 190, 110, 131, 160, 183, 163, 117, 168)

# basic usage:
colPoints(i,j,k, cex=1.5, pch="+", add=FALSE)

# with custom Range (only for method equalinterval):
colPoints(i,j,k, cex=1.5, pch="+", add=FALSE, Range=c(150, 250))
# can be used to allow comparison between several plots
# points outside the range are plottet with col2

# with custom colors:
mycols <- colorRampPalette(c("blue","yellow"))(10)
colPoints(i,j,k, cex=1.5, pch="+", add=FALSE, col=mycols)

# With legend title:
colPoints(i,j,k, cex=2, pch="+", add=FALSE,
         legargs=list(density=FALSE, title="Elevation [m above NN.]"))


# different classification methods:
set.seed(007)
rx <- rnorm(30) ; ry <- rnorm(30) ; rz <- rnorm(30)*100
# sd: normal distribution
mycols <- colorRampPalette(c("green","yellow", "red"))
colPoints(rx,ry,rz, add=FALSE, col=mycols(5), method="s",
          legargs=list(horiz=FALSE, x1=80, x2=95))
colPoints(rx,ry,rz, add=FALSE, col=mycols(6), method="s", sdlab=2,
          legargs=list(horiz=FALSE, labelpos=5, lines=FALSE, title=""))
# quantiles: each color is equally often used
colPoints(rx,ry,rz, add=FALSE, method="q",
          legargs=list(horiz=FALSE, mar=c(0,5,3,0), bg="transparent") )
text(rx,ry,round(rz), col=8)


# With histogram:
colPoints(i,j,k, add=FALSE, hist=NULL)
colPoints(i,j,k, cex=3.5, lwd=3, pch=1, histargs=list(bg=5, breaks=5), add=FALSE)
colPoints(rx,ry,rz, cex=3.5, lwd=3, pch=1, add=FALSE, legargs=FALSE,
   histargs=list(mar=c(0,0,0,0), x1=50,y1=99, x2=100,y2=80, yaxt="n"))

# use classify separately:
text(rx,ry,round(rz), col=mycols(100)[classify(rz)$index], cex=0.7)

# histogram in lower panel:
layout(matrix(1:2), heights=c(8,4) )
colPoints(i,j,k, add=FALSE, legargs=list(y2=80))
colPointsHist(z=k, x1=10,y1=80, x2=100,y2=10)
layout(1)


# Customizing the legend :
colPoints(i,j,k, leg=FALSE, add=FALSE)
colPointsLegend(x1=20,y1=50, x2=95,y2=40, z=k, labelpos=5, atminmax=TRUE, bg=7)
colPointsLegend(x1=50,y1=28, x2=90,y2=18, z=k, Range=c(80, 200), nbins=12, font=3)
colPointsLegend(x1=10,y1=15, x2=40,y2= 5, z=k, labelpos=5, lines=FALSE, title="")

colPointsLegend(z=k, horizontal=FALSE)
colPointsLegend(x1=1, y1=90, z=k, horizontal=FALSE, labelpos=4, cex=1.2)
colPointsLegend(x1=23,y1=95, z=k, horizontal=FALSE, labelpos=5, cex=0.8,
  dens=FALSE, title="", at=c(130,150,170), labels=c("y","rr","Be"), lines=FALSE)
# For method other than colPoints' default, it is easiest to include these
# options as a list in legargs, but you can also use the invisible output
# from colPoints for later calls to colPointsLegend


# colPoints with matrix:
colPoints(z=volcano, add=FALSE)
colPointsHist(z=volcano)
# image and contour by default transpose the matrix! This is really in the data

# highlight local character of points on a regular grid normally drawn with image:
# library(datasets), normally already loaded in newer R versions.
z <- t(volcano)  ;  x <- 1:ncol(z)  ;  y <- 1:nrow(z)
colPoints(x,y,z, add=FALSE)  # takes matrix for z
contour(x,y,t(z), add=TRUE)

# image only takes a regular matrix, but not scatterpoints...
image(x,y,t(z), col=rev(rainbow(100, start=0, end=.7)))

# add single newly measured points to image (fictional data):
mx <- c( 22,  40,  80,  45,  60,  63,  30,  70)
my <- c(  5,  33,  12,  56,  20,  40,  45,  45)
mz <- c(135, 155, 120, 105, 140, 130, 190, 110)
colPoints(mx,my,mz, cex=5, pch="*", Range=c(94, 195), col2=NA, leg=FALSE)
points(mx,my, cex=4)
text(mx,my,mz, adj=-0.5, font=2)
}
\keyword{aplot}
\keyword{hplot}
\keyword{color}
