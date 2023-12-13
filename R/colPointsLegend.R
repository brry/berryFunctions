#' @title Legend for colPoints
#' @description Adds legends to plots created or enhanced with \code{\link{colPoints}}. \cr
#' \code{sf} plots set par(mar=c(0,0,1.2,0)) but then reset it to the values before.
#' \code{\link{smallPlot}} will hence also reset to that, so points added after
#' calling colpointsLegend will be wrong, unless the margins are set BEFORE sf plot.
#' \code{sf:::plot.sf} alternatively uses c(2.1, 2.1, 1.2, 0)  or  c(1, 1, 1.2, 1).
#' @note \code{x1,x2,y1,y2,labelpos,titlepos,title} have different defaults when \code{horizontal=FALSE}\cr
#' @return invisible list of par of \code{\link{smallPlot}}, adds legend bar to current plot
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2012-2014
#' @seealso \code{\link{colPointsHist}}, \code{\link{colPoints}} for real life example
#' @keywords aplot color
#' @importFrom graphics lines par plot.window polygon rect segments strheight strwidth text
#' @importFrom utils head tail
#' @importFrom stats density
#' @export
#' @examples
#' 
#' z <- rnorm(50)
#' plot(1:10)
#' colPointsLegend(z=z)
#' colPointsLegend(z=z, titlepos=2)
#' colPointsLegend(z=z, horiz=FALSE) # note the different defaults
#' # positioning relative to plot:
#' colPointsLegend(z=z, x1=0.05, x2=0.3, y1=0.7,y2=0.9, title="Booh!", density=FALSE)
#' # Denote values outside of Range wit a triangle:
#' colPointsLegend(z=z, Range=c(-1,3), x1=0.2, y1=0.4, y2=0.6, triangle=0.2)
#' colPointsLegend(z=z, horiz=FALSE, x1=0.7, y1=0.6, plottriangle=TRUE, density=FALSE)
#' ?colPoints # example section for actual usage
#' 
#' @param z Values of third dimension used in \code{\link{colPoints}},
#'          can be a matrix or a vector etc, but must be numeric
#' @param Range Ends of color bar for method=equalinterval. DEFAULT: range(z, finite=TRUE)
#' @param nbins Number of classes (thus, colors). If \code{colors} is given,
#'              \code{nbins} is overwritten with \code{length(colors)}. DEFAULT: 100
#' @param colors Color vector. DEFAULT: \code{\link{seqPal}}
#'               from yellow (lowest) to blue (highest value in Range)
#' @param bb Borders of bins for the legend (key). DEFAULT: seqR(Range, length.out=nbins+1)
#' @param nlab,at,labels Number of legend labels, their positions and labels.
#'                    DEFAULT: nlab=5, labels=at=pretty2(Range,nlab)
#' @param atgrey Positions for grey lines with no label, if given. DEFAULT: NULL
#' @param adj label adjustment parallel to legend bar (only one number!). DEFAULT: 0.5
#' @param x1,x2,y1,y2 Relative coordinates [0:1] of inset plot, see \code{\link{smallPlot}}.
#'                    DEFAULT: x: 0.6-0.99, y: 0.88-0.99
#' @param outer Logical: Should legend be relative to device instead of current figure?
#'              use outer=TRUE when par(mfrow, oma) is set. DEFAULT: FALSE
#' @param xpd Logical: should text be expanded outside of plotting region?
#'            Must be NA if outer=TRUE. DEFAULT: NA
#' @param mar Margins for \code{\link{smallPlot}}.
#'            DEFAULT: internal calculations based on title, labelpos and titlepos.
#' @param mgp MarGinPlacement: distance of xlab/ylab, numbers and line from plot margin,
#'            as in \code{\link{par}}, but with different defaults. DEFAULT: c(1.8, 0.6, 0)
#' @param bg Background behind key, labels and title. DEFAULT: par("bg")
#' @param sborder Border around inset subplot. DEFAULT: NA
#' @param resetfocus Reset focus to original plot?
#'                   Specifies where further low level plot commands are directed to. DEFAULT: TRUE
#' @param plottriangle Should triangles be plotted at the end of the legend for values outside Range?
#'                     Vector of length two (for lower and upper, internally recycled).
#'                     If this argument is missing but triangle is given, this is set to TRUE. DEFAULT: FALSE
#' @param triangle Percentage of bar length at lower and upper end for triangles
#'                 (can be a vector with two different values). DEFAULT: 0.14
#' @param tricol Triangle colors for lower and upper end. DEFAULT: c(8,1)
#' @param density List of arguments passed to \code{kernel \link{density} estimation}.
#'                Can also be FALSE to suppress KDE line drawing. DEFAULT: NULL
#' @param lines Plot black lines in the color bar at \code{at}? DEFAULT: TRUE
#' @param atminmax Should the extrema of the legend be added to \code{at}? DEFAULT: FALSE
#' @param horizontal Horizontal bar? if FALSE, a vertical bar is drawn. DEFAULT: TRUE
#' @param labelpos Position of labels relative to the bar.
#'                 Possible: 1 (below), 2 (left), 3 (above), 4 (right), 5(on top of bar). DEFAULT: 1
#' @param titlepos Position of title -"-. DEFAULT: 3
#' @param title Legend title. DEFAULT: "Legend"
#' @param las LabelAxisStyle. DEFAULT: 1
#' @param x,y,index,above,below Ignored arguments, so that you can pass the result from
#'                  \code{\link{colPoints}} via \code{do.call(colPointsLegend, cp_result)}
#' @param \dots Further arguments passed to \code{\link{text}} and \code{\link{strwidth}},
#'         e.g. cex, srt, font, col. But NOT adj!
#' 
colPointsLegend <- function(
z,
Range=range(z, finite=TRUE),
nbins=100,
colors=seqPal(nbins),
bb=seqR(Range, length.out=nbins+1),
nlab=5,
at=pretty2(Range, nlab),
labels=at,
atgrey=NULL,
adj=0.5,

x1=0.6,
y1=0.88,
x2=0.99,
y2=0.99,
outer=FALSE,
xpd=NA,
mar,
mgp=c(1.8, 0.6, 0),
bg=par("bg"),
sborder=NA,
resetfocus=TRUE,

plottriangle=FALSE,
triangle=0.14,
tricol=c(8,1),
density=NULL,
lines=TRUE,
atminmax=FALSE,
horizontal=TRUE,
labelpos=1,
titlepos=3,
title="Legend",
las=1,
x,y,index,above,below,
...)
{
# ------------------------------------------------------------------------------
z <- as.numeric(z)
z <- z[!is.na(z)]
if(length(z)<2) stop("At least 2 non-NA values are needed in 'z', which now is: ",
                     toString(z))
# colors
if(!missing(colors)) nbins <- length(colors)
# input checks:
if(any(diff(bb)<0)) stop("Breaks 'bb' (bin borders) have to be in ascending order.")
if(missing(nbins) & !missing(colors)) nbins <- length(colors)
if(length(colors) != nbins) stop("Number of colors (",length(colors),
                                 ") is not equal to number of classes (",nbins,").")
# extend labels and at:
if(atminmax) labels <- c( signif(head(bb,1),2), labels, signif(tail(bb,1),2) ) ### & length(labels)!=length(at)
if(atminmax) at <- c( head(bb,1), at, tail(bb,1) )
if(length(labels)!=length(at)) stop("length of labels (",length(labels),") and at (",
                                    length(at), ") are not equal.")
# vertical default placement:
if(!horizontal){
if(missing(x1)) x1 <- 0.88
if(missing(y1)) y1 <- 0.30
if(missing(x2)) x2 <- pmin(x1+0.11, 1)
if(missing(y2)) y2 <- pmax(y1+0.40, 0)
if(missing(labelpos)) labelpos <- 2
if(missing(titlepos)) titlepos <- 3
if(missing(title)) title <- "Key"
}
# triangle preparation:
if(!missing(triangle) & missing(plottriangle)) plottriangle <- TRUE
plottriangle <- rep(plottriangle, length.out=2)
if(any(plottriangle))
  {
  if(!is.numeric(triangle)) stop("triangle must be numeric.")
  if(any(triangle>2 | triangle<0)) stop("Values in triangle must be between ",
                                        "0 and 2, not ", toString(triangle))
  triangle <- rep(triangle, length.out=2)
  tricol   <- rep(tricol  , length.out=2)
  # coordinates of triangle points
  barlength <- tail(bb,1) - bb[1]
  trimin <-  bb[1]      - barlength*triangle[1]
  trimax <-  tail(bb,1) + barlength*triangle[2]
  }
plotrange <- c(if(plottriangle[1]) trimin else bb[1],
               if(plottriangle[2]) trimax else tail(bb,1))
# margin preparation:
if(missing(mar))
{
mar <- c(0,0,0,0)
nch <- 1
#if(!horizontal) nch <- max(nchar(c(labels, title)))
if(labelpos==2 | titlepos==2) mar[2] <- nch
if(labelpos==4 | titlepos==4) mar[4] <- nch
if(labelpos==1 | titlepos==1) mar[1] <- nch
if(labelpos==3 | titlepos==3) mar[3] <- nch
} # if mar is specified, it is used, of course.
# subplot setup:
try(smallPlot(x1=x1, y1=y1, x2=x2, y2=y2, outer=outer, xpd=xpd, mar=mar, mgp=mgp,
          bg=bg, border=sborder, las=las, resetfocus=resetfocus, expr={
if(horizontal) # ---------------------------------------------------------------
  {
  plot.window(xlim=plotrange, ylim=c(0,1), xaxs="i", yaxs="i")
  # actually plot legend color fields:
  for(i in 1:length(colors))
    rect(xleft=bb[i], xright=bb[i+1], ybottom=0, ytop=1, col=colors[i], border=NA)
  # triangle:
  if(plottriangle[1]) polygon(c(bb[1],bb[1],trimin),       c(0,1,0.5), col=tricol[1], border=NA)
  if(plottriangle[2]) polygon(c(rep(tail(bb,1),2),trimax), c(0,1,0.5), col=tricol[2], border=NA)
  # lines
  if(lines & !is.null(atgrey)) segments(x0=atgrey, y0=0, y1=1, col="grey60")
  if(lines) segments(x0=at, y0=0, y1=1)
  # prepare label adjustment:
  if(labelpos==1) { y <- -0.1 ; vadj <- 1   } else
  if(labelpos==3) { y <-  1.1 ; vadj <- 0   } else
  if(labelpos==5) { y <-  0.5 ; vadj <- 0.5 } else
  stop("Wrong labelpos (",labelpos,"). Possible in horizontal legend: ",
       "1 (below legend bar), 3 (above), and 5 (on top).")
  # actually write labels:
  text(x=at, y=y, labels=labels, adj=c(adj, vadj), xpd=xpd, ...)
  # prepare title adjustment:
  pu <- par("usr")[1:2]
  if(titlepos==1) {x <- mean(pu); y <- -0.2; hadj <- 0.5; vadj <- 1   } else
  if(titlepos==2) {x <-    pu[1]; y <-  0.5; hadj <- 1  ; vadj <- 0.5 } else
  if(titlepos==3) {x <- mean(pu); y <-  1.2; hadj <- 0.5; vadj <- 0   } else
  if(titlepos==4) {x <-    pu[2]; y <-  0.5; hadj <- 0  ; vadj <- 0.5 } else
  if(titlepos==5) {x <- mean(pu); y <-  0.5; hadj <- 0.5; vadj <- 0.5 } else
  stop("Wrong titlepos (",titlepos,"). Must be integer between 1 and 5.")
  # actually write title:
  text(x=x, y=y, labels=title, adj=c(hadj, vadj), xpd=xpd, ...)
  # kernel density:
  if(is.list(density) | is.null(density) )
    {
    dp <- do.call(stats::density, args=owa(list(x=z, from=min(z), to=max(z)), density))
    lines(dp$x, dp$y/max(dp$y))
    }
  }
else # if not horizontal, thus if vertical -------------------------------------
  {
  plot.window(ylim=plotrange, xlim=c(0,1), xaxs="i", yaxs="i")
  # actually plot legend color fields:
  for(i in 1:length(colors))
    rect(ybottom=bb[i], ytop=bb[i+1], xleft=0, xright=1, col=colors[i], border=NA)
  # triangle:
  if(plottriangle[1]) polygon(c(0,1,0.5), c(bb[1],bb[1],trimin),       col=tricol[1], border=NA)
  if(plottriangle[2]) polygon(c(0,1,0.5), c(rep(tail(bb,1),2),trimax), col=tricol[2], border=NA)
  # lines
  if(lines & !is.null(atgrey)) segments(y0=atgrey, x0=0, x1=1, col="grey60")
  if(lines) segments(y0=at, x0=0, x1=1)
  # prepare label adjustment:
  if(labelpos==2) { x <- -0.1 ; hadj <- 1   } else
  if(labelpos==4) { x <-  1.1 ; hadj <- 0   } else
  if(labelpos==5) { x <-  0.5 ; hadj <- 0.5 } else
  stop("Wrong labelpos (",labelpos,"). Possible in vertical legend: ",
       "2 (left of legend bar), 4 (right), and 5 (on top).")
  # actually write labels:
  text(x=x, y=at, labels=labels, adj=c(hadj, adj), xpd=xpd, ...)
  # prepare title adjustment:
  pu <- par("usr")[3:4]
  if(titlepos==1) {y <-    pu[1]; x <-  0.5; hadj <- 0.5; vadj <- 1  } else
  if(titlepos==2) {y <- mean(pu); x <- -0.2; hadj <- 1  ; vadj <- 0.5} else
  if(titlepos==3) {y <-    pu[2]; x <-  0.5; hadj <- 0.5; vadj <- -0.2} else
  if(titlepos==4) {y <- mean(pu); x <-  1.2; hadj <- 0  ; vadj <- 0.5} else
  if(titlepos==5) {y <- mean(pu); x <-  0.5; hadj <- 0.5; vadj <- 0.5} else
  stop("Wrong titlepos (",titlepos,"). Must be integer between 1 and 5.")
  # actually write title:
  text(x=x, y=y, labels=title, adj=c(hadj, vadj), xpd=xpd, ...)
    # kernel density:
  if(is.list(density) | is.null(density) )
    {
    dp <- do.call(stats::density, args=owa(list(x=z, from=min(z), to=max(z), bw="SJ"), density))
    lines(y=dp$x, x=dp$y/max(dp$y))
    }
  } # end vertical -------------------------------------------------------------
  })) # end smallPlot
}
