#' Histogram for colPoints
#' 
#' Adds Histogram to plots created or enhanced with \code{\link{colPoints}}
#' 
#' @return invisible list of par of smallPlot, adds histogram to current plot
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2014
#' @seealso \code{\link{colPointsLegend}} and \code{\link{colPoints}} for real life examples
#' @keywords aplot color
#' @importFrom graphics axis hist par rect
#' @export
#' @examples
#' 
#' z <- rnorm(50)
#' plot(1:10)
#' colPointsHist(z=z)
#' 
#' @param z Values of third dimension used in \code{\link{colPoints}}
#' @param nbins Number of classes (thus, colors). DEFAULT: 40
#' @param colors Colors that are used for the background. DEFAULT: seqPal(nbins)
#' @param bb Borders of bins for the background. DEFAULT: seqR(z, length.out=nbins+1)
#' @param at Positions of x-axis labels. DEFAULT: pretty2(z)
#' @param labels X-axis labels themselves. DEFAULT: at
#' @param bg Background behind background and axis labels. DEFAULT: "white"
#' @param x1,x2,y1,y2 Relative coordinates [0:1] of inset plot, see \code{\link{smallPlot}}.
#'        DEFAULT: x: 0-0.3, y: 0-0.4
#' @param outer Logical: Should legend be relative to device instead of current figure?
#'              use outer=TRUE when par(mfrow, oma) is set. DEFAULT: FALSE
#' @param mar Margins for \code{\link{smallPlot}}. DEFAULT: c(2, 2, 1, 0.5)
#' @param mgp MarGinPlacement: distance of xlab/ylab, numbers and line from plot margin,
#'        as in \code{\link{par}}, but with different defaults. DEFAULT: c(1.8, 0.6, 0)
#' @param sborder Border around inset subplot. DEFAULT: par("fg")
#' @param resetfocus Reset focus to original plot? Specifies where further
#'        low level plot commands are directed to. DEFAULT: TRUE
#' @param breaks Breaks as in \code{\link{hist}}, but with a different default. DEFAULT: 20
#' @param freq Plot count data in hist? (if FALSE, plot density instead). DEFAULT: TRUE
#' @param col Color of histogram bars. DEFAULT: par("fg")
#' @param border Border around each bar. DEFAULT: NA
#' @param main,ylab,xlab Labels. DEFAULT: ""
#' @param las LabelAxisStyle. DEFAULT: 1
#' @param axes Draw axes?. DEFAULT: TRUE
#' @param \dots Further arguments passed to \code{\link{hist}}. NOT POSSIBLE: \code{x, add}
#' 
colPointsHist <- function(
z,
nbins=40,
colors=seqPal(nbins),
bb=seqR(z, length.out=nbins+1),
at=pretty2(z),
labels=at,

bg="white",
x1=0, x2=0.4,
y1=0, y2=0.3,
outer=FALSE,
mar=c(2,2,1,0.5),
mgp=c(1.8, 0.6, 0),
sborder=NA,
resetfocus=TRUE,

breaks=20,
freq=TRUE,
col=par("fg"),
border=NA,
main="", ylab="", xlab="",
las=1,
axes=TRUE,
...)
{
z <- as.numeric(z)
if(length(colors) != nbins) stop("Number of colors is not equal to number of classes.")
# plot setup:
smallPlot(x1=x1,y1=y1, x2=x2,y2=y2, outer=outer, mar=mar, mgp=mgp, bg=bg,
  border=sborder, las=las, resetfocus=resetfocus, expr={
  hist(z, breaks=breaks, main=main, xaxt="n", ylab=ylab, xlab=xlab,
             freq=freq, las=las, col=col, border=border, ...)
  if(axes) axis(side=1, at=at, labels=labels, lwd=0, lwd.ticks=1)
  # colored background:
  for(i in 1:length(colors) )
    rect(xleft=bb[i], ybottom=par("usr")[3],
        xright=bb[i+1],  ytop=par("usr")[4], col=colors[i], border=NA)
  # overdraw with black histogram:
  hist(z, breaks=breaks, freq=freq, add=TRUE, col=col, border=border, ...)
  })
}
