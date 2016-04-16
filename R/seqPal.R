#' Sequential color palette
#' 
#' Sequential color palette from yelow to red or yellow to blue or custom colors.
#' 
#' @return Character string vector with color names
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2016
#' @seealso \code{\link{divPal}}, \code{\link{addAlpha}}, \code{\link{colorRampPalette}},
#' package RColorBrewer, Why not use rainbow? see e.g. \url{https://eagereyes.org/basics/rainbow-color-map}
#' @keywords color dplot
#' @export
#' @examples
#' 
#' op <- par(mfrow=c(11,1), mar=c(0,0,0,0))
#' for(n in c(3,7,11,12,300)) plot(rep(1,n), pch=16, cex=5, col=seqPal(n), xaxt="n")
#' text(150, 1, "berryFunctions::seqPal default palette")
#' plot(rep(1,300), pch=16, cex=5, col=seqPal(300, extr=TRUE), xaxt="n")
#' text(150, 1, "extr=TRUE")
#' plot(rep(1,12),  pch=16, cex=5, col=seqPal(alpha=0.4), xaxt="n")
#' text(6, 1, "semi-transparency with alpha=0.4")
#' plot(rep(1,12),  pch=16, cex=5, col=seqPal(rev=TRUE), xaxt="n")
#' plot(rep(1,300), pch=16, cex=5, col=seqPal(300,yb=TRUE), xaxt="n")
#' plot(rep(1,300), pch=16, cex=5, col=seqPal(300,yr=TRUE), xaxt="n")
#' plot(rep(1,300), pch=16, cex=5, col=seqPal(300,col=c("darkblue","green","orange")), xaxt="n")
#' par(op)
#' 
#' @param n Number of colors. DEFAULT: 12
#' @param reverse Reverse colors? DEFAULT: FALSE
#' @param alpha Transparency (0=transparent, 1=fully colored). DEFAULT: 1
#' @param extr Should colors span possible range more extremely? 
#'       If TRUE, it has very light yellow and very dark blue values included, 
#'       using the result from \code{RColorBrewer::brewer.pal(9, "YlGnBu")}. DEFAULT: FALSE
#' @param yb Should colors be in yellow-blue instead of the internal (nice) default? DEFAULT: FALSE
#' @param yr Should colors be in yellow-red instead of the default? DEFAULT: FALSE
#' @param colors If not NULL, a color vector used in \code{\link{colorRampPalette}}. DEFAULT: NULL
#' @param \dots Further arguments passed to \code{\link{colorRamp}}
#' 
seqPal <- function(
n=12,
reverse=FALSE,
alpha=1,
extr=FALSE,
yb=FALSE,
yr=FALSE,
colors=NULL,
...
)
{
cols <- c("#FFFFC6","#CAE9AE","#85CDBA","#4DB6C6","#327EBD","#22329A")
if(extr) cols <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4",
                    "#1D91C0", "#225EA8", "#253494", "#081D58")
if(yb) cols <- c("yellow","blue")
if(yr) cols <- c("yellow","red")
if(!is.null(colors)) cols <- colors
if(reverse) cols <- rev(cols)
outcols <- colorRampPalette(cols)(n)
if(alpha!=1) outcols <- addAlpha(outcols, alpha)
outcols
}
