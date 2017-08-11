#' Sequential color palette
#' 
#' Sequential color palette from yelow to red or yellow to blue or custom colors.
#' 
#' @return Character string vector with color names
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2016
#' @seealso \code{\link{showPal}}, \code{\link{divPal}}, \code{\link{addAlpha}},
#'          \code{\link{colorRampPalette}}, package \code{RColorBrewer}
#' @keywords color dplot
#' @importFrom grDevices colorRampPalette
#' @export
#' @examples
#' 
#' plot(rep(1,12),  pch=16, cex=5, col=seqPal(12), xaxt="n")
#' showPal()
#' 
#' # nonlinear color scale (use colPoints + see classify for more options):
#' v <- rescale(volcano^30)
#' image(v, col=seqPal(1000), asp=1);  colPointsLegend(v, nbins=1000)
#' image(v, col=seqPal(1000, logbase=1.007), asp=1)
#' colPointsLegend(v, col=seqPal(1000, logbase=1.09))
#' 
#' plot(    rep(1, 1000), pch=15, cex=3, col=seqPal(1000), ylim=c(0.99, 1.01), ylab="logbase", las=1)
#' for(b in seq(0.99, 1.01, len=30))
#'     points(rep(b, 1000), pch=15, cex=1, col=seqPal(1000, logbase=b))
#' 
#' @param n Number of colors. DEFAULT: 12
#' @param reverse Reverse colors? DEFAULT: FALSE
#' @param alpha Transparency (0=transparent, 1=fully colored). DEFAULT: 1
#' @param extr Should colors span possible range more extremely?
#'       If TRUE, it has very light yellow and very dark blue values included,
#'       using the result from \code{RColorBrewer::brewer.pal(9, "YlGnBu")}. DEFAULT: FALSE
#' @param yb Should colors be in yellow-blue instead of the internal (nice) default? DEFAULT: FALSE
#' @param yr Should colors be in yellow-red instead of the default? DEFAULT: FALSE
#' @param gb Should colors be in green-blue instead of the default? DEFAULT: FALSE
#' @param b  Should colors be in an increasingly saturated blue? DEFAULT: FALSE
#' @param colors If not NULL, a color vector used in \code{\link{colorRampPalette}}. DEFAULT: NULL
#' @param logbase If \code{!=1}, this is passed to \code{\link{classify}} and \code{\link{logSpaced}}. DEFAULT: 1
#' @param \dots Further arguments passed to \code{\link{colorRamp}}
#' 
seqPal <- function(
n=12,
reverse=FALSE,
alpha=1,
extr=FALSE,
yb=FALSE,
yr=FALSE,
gb=FALSE,
b=FALSE,
colors=NULL,
logbase=1,
...
)
{
cols <- c("#FFFFC6","#CAE9AE","#85CDBA","#4DB6C6","#327EBD","#22329A")
if(extr) cols <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4",
                    "#1D91C0", "#225EA8", "#253494", "#081D58")
if(yb) cols <- c("yellow","blue")
if(yr) cols <- c("yellow","red")
if(gb) cols <- c("chartreuse","cornflowerblue","darkblue")
if(b)  cols <- c("#BADEF4","#3D6088")
if(!is.null(colors)) cols <- colors
if(reverse) cols <- rev(cols)
outcols <- colorRampPalette(cols)(n)
if(logbase!=1)
  {
  n1 <- n#*10 # log(90, base=logbase)
  cl <- classify(1:n1, method="logspaced", breaks=c(n1,logbase))
  n2 <- cl$nbins
  outcols <- colorRampPalette(cols)(n2)[cl$index]
  # message("n: ", round(n1), ", number of bins from classify: ", n2, ", number of unique colors: ", length(unique(outcols)))
  }
if(alpha!=1) outcols <- addAlpha(outcols, alpha)
outcols
}


