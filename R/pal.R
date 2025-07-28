# + showPal ----

#' @title show color palettes
#' @description 
#' Plot examples of the sequential and diverging color palettes in this package.
#' Do not use \code{rainbow}: \url{https://eagereyes.org/basics/rainbow-color-map}
#' @return NULL
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr 2016
#' @seealso \code{\link{seqPal}}, \code{\link{divPal}}, \code{\link{catPal}}, 
#' package \code{RColorBrewer}, 
#' \url{https://www.datawrapper.de/blog/which-color-scale-to-use-in-data-vis/}
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
op <- par(mfcol=c(9,2), mar=c(0,0,0,0), oma=c(0,0,1.8,0), yaxt="n", xaxt="n", ...)
on.exit(par(op), add=TRUE)
pal <- function(cols, txt)
 {
 n <- length(cols)
 plot(rep(1, n), pch=15, cex=cex, col=cols)
 text(par("usr")[1] + 0.05*diff(par("usr")[1:2]), 1, txt, adj=0) # text(n/2+0.5, 1, txt)
 }
# Sequential palette -----------------------------------------------------------
pal(seqPal()               , "default, n=100")
pal(seqPal(extr=TRUE)      , "extr=TRUE")
pal(seqPal(alpha=0.4, n=12), "alpha=0.4 (semi-transparency)")
pal(seqPal(reverse=TRUE)   , "rev=TRUE")
pal(seqPal(yb=TRUE)        , "yb=TRUE")
pal(seqPal(yr=TRUE)        , "yr=TRUE")
pal(seqPal(gb=TRUE)        , "gb=TRUE")
pal(seqPal( b=TRUE)        , "b=TRUE")
pal(seqPal(colors=c("orange","green","darkblue")), 
    'col=c("orange","green","darkblue"))')
title(main="berryFunctions::seqPal", xpd=NA, outer=TRUE, adj=0.2, line=0.5)

# Diverging palette ------------------------------------------------------------
pal(divPal()               , "default")
pal(divPal(bias=0.5)       , "bias=0.5")
pal(divPal(alpha=0.4, n=12), "alpha=0.4, n=12")
pal(divPal(rwb=TRUE)       , "rwb=TRUE")
pal(divPal(ryb=TRUE)       , "ryb=TRUE")
pal(divPal( gp=TRUE)       , "gp=TRUE")
pal(divPal( br=TRUE)       , "br=TRUE")
title(main="berryFunctions::divPal", xpd=NA, outer=TRUE, adj=0.8, line=0.5)

plot.new()
pal(catPal(), "")
title(main="berryFunctions::catPal", xpd=NA, line=0.5)
}



# + catPal ----

#' @title Categorical color palette
#' @description 
#' Categorical color palette according to IwantHue as displayed on
#' \url{https://web.archive.org/web/20250122084330/https://rockcontent.com/blog/subtleties-of-color-different-types-of-data-require-different-color-schemes/}
#' @return Character string vector with color names
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr 2019
#' @seealso \code{\link{showPal}}, \code{\link{seqPal}}, \code{\link{divPal}}
#' @keywords color dplot
#' @export
#' @importFrom utils read.table
#' @examples
#' plot(rep(1,12), pch=16, cex=5, col=catPal(12), xaxt="n")
#' showPal()
#' plot(cumsum(rnorm(40)), type="l", col=catPal()[1], ylim=c(-10,10))
#' for(i in 2:6) lines(cumsum(rnorm(40)), col=catPal()[i])
#' @param n Number of colors, max 12. DEFAULT: 12
#' @param set Integer for which set to use. Currently, only 1 is implemented.
#' @param alpha Transparency (0=transparent, 1=fully colored). DEFAULT: 1
catPal <- function(
n=12,
set=1,
alpha=1
)
{
if(set==1){
RGB <- read.table(header=TRUE, sep=",", text="
r,g,b
132, 213, 164
192,  89, 203
208,  83,  61
 65,  83,  84
206, 169,  83
145, 212,  75
205,  91, 137
168, 182, 192
121, 126, 203
 86, 117,  57
114,  66,  47
 93,  55,  98")
cols <- rgb(RGB$r, RGB$g, RGB$b, maxColorValue=255)}
else stop("Currently, only set=1 is available.")
if(n>12) n <- 12
if(n<1)  n <- 1
outcols <- cols[1:n]
if(alpha!=1) outcols <- addAlpha(outcols, alpha)
outcols
}



# + divPal ----

#' @title Diverging color palette
#' @description 
#' Diverging color palette: brown to blue, light colors in the middle, darker
#' at the extremes, good for displaying values in two directions
#' @return Character string vector with color names
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2016
#' @seealso \code{\link{showPal}}, \code{\link{seqPal}}, \code{\link{catPal}}, \code{\link{addAlpha}},
#'          \code{\link{colorRampPalette}}, package \code{RColorBrewer}
#' @references The default palette is originally in 12 shades in the IPCC Assessment Report 5
#'             Chapter 12 Fig 12.22, \url{https://www.ipcc.ch/report/ar5/wg1/}.\cr
#'             The green-purple and blue-red palettes are from NYtimes (originally with 8 shades),
#'             \url{https://www.nytimes.com/interactive/2017/03/21/climate/how-americans-think-about-climate-change-in-six-maps.html}
#' @keywords color dplot
#' @importFrom grDevices colorRampPalette
#' @export
#' @examples
#' plot(rep(1,12), pch=16, cex=5, col=divPal(12), xaxt="n")
#' showPal()
#' @param n Number of colors. DEFAULT: 100
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
n=100,
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
#' @seealso \code{\link{showPal}}, \code{\link{divPal}}, \code{\link{catPal}}, \code{\link{addAlpha}},
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
#' @param n Number of colors. DEFAULT: 100
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
n=100,
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
