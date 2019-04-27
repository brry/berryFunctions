# + showPal ----

#' @title show color palettes
#' @description 
#' Plot examples of the sequential and diverging color palettes in this package.
#' Do not use \code{rainbow}: \url{https://eagereyes.org/basics/rainbow-color-map}
#' @return NULL
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr 2016
#' @seealso \code{\link{seqPal}}, \code{\link{divPal}}, package \code{RColorBrewer}
#' @keywords dplot color
#' @importFrom graphics par plot text title
#' @export
#' @examples
#' showPal()
#' @param cex Character EXpansion size (width of color bar). DEFAULT: 4
#' @param \dots Arguments passed to \code{\link{par}}
#' 
showPal <- function(cex=4,...)
{
op <- par(mfcol=c(11,2), mar=c(0,0,0,0), oma=c(0,0,1.8,0), yaxt="n", xaxt="n", ...)
on.exit(par(op), add=TRUE)
# Sequential palette -----------------------------------------------------------
plot(rep(1, 12), pch=15, cex=cex, col=seqPal(12))            ; text(  6, 1, "default")
plot(rep(1,  7), pch=15, cex=cex, col=seqPal(7))             ; text(  3, 1, "n=7 works, too")
plot(rep(1,300), pch=15, cex=cex, col=seqPal(300))           ; text(150, 1, "n=300")
plot(rep(1,300), pch=15, cex=cex, col=seqPal(300, extr=TRUE)); text(150, 1, "extr=TRUE")
plot(rep(1, 12), pch=15, cex=cex, col=seqPal(alpha=0.4))     ; text(  6, 1, "alpha=0.4 (semi-transparency)")
plot(rep(1, 12), pch=15, cex=cex, col=seqPal(reverse=TRUE))  ; text(  6, 1, "rev=TRUE")
plot(rep(1,300), pch=15, cex=cex, col=seqPal(300, yb=TRUE))  ; text(150, 1, "yb=TRUE")
plot(rep(1,300), pch=15, cex=cex, col=seqPal(300, yr=TRUE))  ; text(150, 1, "yr=TRUE")
plot(rep(1,300), pch=15, cex=cex, col=seqPal(300, gb=TRUE))  ; text(150, 1, "gb=TRUE")
plot(rep(1,300), pch=15, cex=cex, col=seqPal(300,  b=TRUE))  ; text(150, 1, "b=TRUE")
plot(rep(1,300), pch=15, cex=cex, col=rainbow2(300))         ; text(150, 1, "berryFunctions::rainbow2(300)", font=2)
title(main="berryFunctions::seqPal", xpd=NA, outer=TRUE, adj=0.2, line=0.5)

# Diverging palette ------------------------------------------------------------
plot(rep(1, 12), pch=15, cex=cex, col=divPal(12))            ; text(  6, 1, "default")
plot(rep(1,  7), pch=15, cex=cex, col=divPal(7))             ; text(  3, 1, "n=7")
plot(rep(1,300), pch=15, cex=cex, col=divPal(300))           ; text(150, 1, "n=300")
plot(rep(1,300), pch=15, cex=cex, col=divPal(300, bias=0.5)) ; text(150, 1, "bias=0.5")
plot(rep(1, 12), pch=15, cex=cex, col=divPal(alpha=0.4))     ; text(  6, 1, "alpha=0.4 (semi-transparency)")
plot(rep(1, 12), pch=15, cex=cex, col=divPal(reverse=TRUE))  ; text(  6, 1, "rev=TRUE")
plot(rep(1,300), pch=15, cex=cex, col=divPal(300, rwb=TRUE)) ; text(150, 1, "rwb=TRUE")
plot(rep(1,300), pch=15, cex=cex, col=divPal(300, ryb=TRUE)) ; text(150, 1, "ryb=TRUE")
plot(rep(1,300), pch=15, cex=cex, col=divPal(300,  gp=TRUE)) ; text(150, 1, "gp=TRUE")
plot(rep(1,300), pch=15, cex=cex, col=divPal(300,  br=TRUE)) ; text(150, 1, "br=TRUE")
plot(rep(1,300), pch=15, cex=cex, col=seqPal(300,colors=c("darkblue","green","orange")))
text(150, 1, 'col=c("darkblue","green","orange"))')
title(main="berryFunctions::divPal", xpd=NA, outer=TRUE, adj=0.8, line=0.5)
}



# + divPal ----

#' @title Diverging color palette
#' @description 
#' Diverging color palette: brown to blue, light colors in the middle, darker
#' at the extremes, good for displaying values in two directions
#' @return Character string vector with color names
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2016
#' @seealso \code{\link{showPal}}, \code{\link{seqPal}}, \code{\link{addAlpha}},
#'          \code{\link{colorRampPalette}}, package \code{RColorBrewer}
#' @references The default palette is originally in 12 shades in the IPCC Assessment Report 5
#'             Chapter 12 Fig 12.22, \url{http://www.ipcc.ch/report/ar5/wg1/}.\cr
#'             The green-purple and blue-red palettes are from NYtimes (originally with 8 shades),
#'             \url{https://nyti.ms/2mL0o4J}
#' @keywords color dplot
#' @importFrom grDevices colorRampPalette
#' @export
#' @examples
#' plot(rep(1,12), pch=16, cex=5, col=divPal(12), xaxt="n")
#' showPal()
#' @param n Number of colors. DEFAULT: 12
#' @param reverse Reverse colors? DEFAULT: FALSE
#' @param alpha Transparency (0=transparent, 1=fully colored). DEFAULT: 1
#' @param rwb Should colors be in red-white-blue instead of brown-blue? DEFAULT: FALSE
#' @param ryb Use red-yellow-blue instead of the default, with "khaki" in the center. DEFAULT: FALSE
#' @param gp Use green-purple instead of the default. DEFAULT: FALSE
#' @param br Use blue-red instead of the default. DEFAULT: FALSE
#' @param colors If not NULL, a color vector used in \code{\link{colorRampPalette}}. DEFAULT: NULL
#' @param \dots Further arguments passed to \code{\link{colorRamp}}
#' 
divPal <- function(
n=12,
reverse=FALSE,
alpha=1,
rwb=FALSE,
ryb=FALSE,
gp=FALSE,
br=FALSE,
colors=NULL,
...
)
{
cols <- c("#9B5523", "#B16A32", "#CA8448", "#F4C882", "#F1DB99", "#FBF5B4",
          "#C3E2C0", "#96D1A7", "#46BEA0", "#4984A0", "#4984A0", "#0B3A5B")
if(rwb) cols <- c("red","white","blue")
if(ryb) cols <- c("red","khaki1","blue")
if(gp) cols <- c("#859B12","#A0AF52","#BCBF8B","#DBCFB9","#D5C7CC","#C593B8","#AB78A2","#8A4683")
if(br) cols <- c("#3F797F","#6498A0","#8BB7B5","#BED8D9","#FCC6A9","#E69A6E","#DC6E3C","#BC4231")
if( (gp|br) & missing(n) ) n <- 8
if(!is.null(colors)) cols <- colors
if(reverse) cols <- rev(cols)
outcols <- colorRampPalette(cols, ...)(n)
if(alpha!=1) outcols <- addAlpha(outcols, alpha)
outcols
}



# + seqPal ----

#' @title Sequential color palette
#' @description Sequential color palette from yellow to blue or custom colors.
#' @return Character string vector with color names
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2016
#' @seealso \code{\link{showPal}}, \code{\link{divPal}}, \code{\link{addAlpha}},
#'          \code{\link{colorRampPalette}}, package \code{RColorBrewer}
#' @keywords color dplot
#' @importFrom grDevices colorRampPalette
#' @export
#' @examples
#' plot(rep(1,12),  pch=16, cex=5, col=seqPal(12), xaxt="n")
#' showPal()
#' 
#' # nonlinear color scale (use colPoints + see classify for more options):
#' v <- rescale(volcano^30)
#' image(v, col=seqPal(1000), asp=1);  colPointsLegend(v, nbins=1000)
#' image(v, col=seqPal(1000, logbase=1.007), asp=1)
#' colPointsLegend(v, col=seqPal(1000, logbase=1.007))
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
outcols <- colorRampPalette(cols, ...)(n)
if(logbase!=1)
  {
  n1 <- n#*10
  cl <- classify(1:n1, method="log", breaks=n1, logbase=logbase)
  n2 <- cl$nbins
  outcols <- colorRampPalette(cols, ...)(n2)[cl$index]
  # message("n: ", round(n1), ", number of bins from classify: ", n2, ", number of unique colors: ", length(unique(outcols)))
  }
if(alpha!=1) outcols <- addAlpha(outcols, alpha)
outcols
}


