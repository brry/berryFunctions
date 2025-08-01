#' Points colored relative to third dimension
#' 
#' Draw colored points for 3D-data in a 2D-plane. Color is relative to third
#' dimension, by different classification methods. Can take 3 vectors or, as in
#' \code{\link{image}}, 2 vectors and a matrix for z.\cr
#' Adding points after \code{\link{smallPlot}} is called for the legend may be
#' incorrect if the original function messes with the graph margins,
#' see the note in \code{\link{colPointsLegend}}.
#' 
#' @return Invisible list of values that can be passed to colPointsLegend or colPointsHist.
#' @note Rstudio scales graphics really badly, so don't expect the right legend width out of the box if you use Rstudio!
#'      Exporting via \code{png("myplot.png", 600,400); colPoints(x,y,z); dev.off()} usually works much better
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2011-2014. I'd be interested in hearing what you used the function for.
#' @seealso \code{\link{classify}}, \code{\link{colPointsLegend}}, \code{\link{colPointsHist}}
#' @references \url{https://archive.ph/EL9Vq},
#'             \url{https://www.theusrus.de/blog/the-good-the-bad-22012/}
#' @keywords aplot hplot color
#' @importFrom grDevices colorRampPalette rainbow
#' @importFrom graphics plot points segments abline image
#' @importFrom stats approx median na.omit
#' @export
#' @examples
#' 
#' i <- c( 22,  40,  48,  60,  80,  70,  70,  63,  55,  48,  45,  40,  30,  32)
#' j <- c(  5,  10,  15,  20,  12,  30,  45,  40,  30,  36,  56,  33,  45,  23)
#' k <- c(175, 168, 163, 132, 120, 117, 110, 130, 131, 160, 105, 174, 190, 183)
#' 
#' # basic usage:
#' colPoints(i,j,k, cex=1.5, pch="+", add=FALSE)
#' 
#' # with custom Range:
#' colPoints(i,j,k, cex=1.5, pch="+", add=FALSE, Range=c(150,190), density=FALSE)
#' # can be used to allow comparison between several plots
#' # points outside the range are plotted with col2
#' 
#' # with custom colors:
#' mycols <- colorRampPalette(c("blue","yellow","red"))(50)
#' colPoints(i,j,k, cex=1.5, pch="+", add=FALSE, col=mycols)
#' 
#' # With legend title:
#' colPoints(i,j,k, cex=2, add=FALSE, zlab="Elevation [m above NN.]",
#'          legargs=list(density=FALSE))
#' ?colPointsLegend # to see which arguments can be set via legargs
#' 
#' 
#' # colPoints with matrix:
#' colPoints(z=volcano, add=FALSE)
#' # image and contour by default transpose and reverse the matrix!
#' # colPoints shows what is really in the data.
#' 
#' # add single newly measured points to image (fictional data):
#' mx <- c( 22,  40,  45,  30,  30,  10)
#' my <- c(  5,  33,  56,  70,  45,  45)
#' mz <- c(110, 184, 127, 133, 170, 114)
#' colPoints(mx,my,mz, cex=5, pch="*", Range=c(94, 195), col=seqPal(), col2=NA, legend=FALSE)
#' points(mx,my, cex=4)
#' text(mx,my,mz, adj=-0.5, font=2)
#' 
#' 
#' # with logarithmic color scale:
#' shp <- seq(0.2,3, by=0.1)
#' scl <- seq(0.2,3, by=0.1)
#' wsim <- sapply(shp, function(h) sapply(scl, function(c) mean(rweibull(1e3, shape=h, scale=c))))
#' colPoints(shp, scl, (wsim), add=FALSE, asp=1)
#' colPoints(shp, scl, (wsim), add=FALSE, asp=1, method="log")
#' 
#' 
#' # with lines (nint to change number of linear interpolation points):
#' colPoints(i,j,k, cex=1.5, add=FALSE, lines=TRUE, nint=10, lwd=2)
#' # With NAs separating lines:
#' tfile <- system.file("extdata/rivers.txt", package="berryFunctions")
#' rivers <- read.table(tfile, header=TRUE, dec=",")
#' colPoints(x,y,n, data=rivers, add=FALSE, lines=TRUE)
#' colPoints(x,y,n, data=rivers, add=FALSE, lines=TRUE, pch=3, lwd=3)
#' colPoints(x,y,n, data=rivers, add=FALSE, lines=TRUE, pch=3, lwd=3, nint=2)
#' colPoints("x","y","n", data=rivers, add=FALSE)
#' 
#' # different classification methods:
#' # see ?classify
#' 
#' colPoints(i,j,k, add=FALSE) # use classify separately:
#' text(i,j+1,k, col=divPal(100,rev=TRUE)[classify(k)$index], cex=1)
#' 
#' 
#' # Add histogram:
#' cp <- colPoints(i,j,k, add=FALSE)
#' do.call(colPointsHist, cp[c("z","at","labels","bb","nbins")])
#' do.call(colPointsHist, owa(cp[c("z","at","labels","bb","nbins")],
#'                            list(bg=5, breaks=5)))
#' do.call(colPointsHist, owa(cp[c("z","at","labels","bb","nbins")],
#'                            list(mar=c(0,0,0,0), x1=0.5, x2=1, y1=0.8,
#'                              y2=0.99, yaxt="n")))
#' # histogram in lower panel:
#' layout(matrix(1:2), heights=c(8,4) )
#' colPoints(i,j,k, add=FALSE, y1=0.8, y2=1)
#' colPointsHist(z=k, x1=0.05, x2=1, y1=0, y2=0.4, mar=3, outer=TRUE)
#' layout(1)
#' 
#' 
#' # Customizing the legend :
#' cp <- colPoints(i,j,k, legend=FALSE, add=FALSE)
#' colPointsLegend(x1=0.2, x2=0.95, y1=0.50, y2=0.40, z=k, labelpos=5, atminmax=TRUE, bg=7)
#' colPointsLegend(x1=0.5, x2=0.90, y1=0.28, y2=0.18, z=k, Range=c(80, 200), nbins=12, font=3)
#' colPointsLegend(x1=0.1, x2=0.40, y1=0.15, y2=0.05, z=k, labelpos=5, lines=FALSE, title="")
#' colPointsLegend(z=k, horizontal=FALSE)
#' colPointsLegend(x1=0.01, y2=0.80, z=k, horizontal=FALSE, labelpos=4, cex=1.2)
#' colPointsLegend(x1=0.23, y2=0.95, z=k, horizontal=FALSE, labelpos=5, cex=0.8,
#'   dens=FALSE, title="", at=c(130,150,170), labels=c("y","rr","Be"), lines=FALSE)
#' # For method other than colPoints' default, it is easiest to include these
#' # options as a list in legargs, but you can also use the invisible output
#' # from colPoints for later calls to colPointsLegend
#' do.call(colPointsLegend, cp)
#' do.call(colPointsLegend, owa(cp, list(colors=divPal(100), cex=1.2)))
#' 
#' 
#' # santiago.begueria.es/2010/10/generating-spatially-correlated-random-fields-with-r
#' if(require(gstat)){
#' xyz <- gstat(formula=z~1, locations=~x+y, dummy=TRUE, beta=1,
#'              model=vgm(psill=0.025,model="Exp",range=5), nmax=20)
#' xyz <- predict(xyz, newdata=data.frame(x=runif(200, 20,40),y=runif(200, 50,70)), nsim=1)
#' head(xyz)
#' colPoints(x,y,sim1, data=xyz, add=FALSE)
#' }
#' 
#' @param x,y      Vectors with coordinates of the points to be drawn
#' @param z        z values belonging to coordinates.
#'                 Vector or matrix with the color-defining height values
#' @param data     Optional: data.frame with the column names as given by x,y and z.
#' @param add      Logical. Should the points be added to current (existing!) plot?
#'                 If FALSE, a new plot is started.
#'                 DEFAULT: TRUE (It's called col\bold{Points}, after all)
#' @param col      Vector of colors to be used. DEFAULT: 100 colors from sequential
#'                 palette \code{\link{seqPal}} (color-blind safe, black/white-print safe)
#' @param col2     Color for points where z is NA, or lower / higher than \code{Range}.
#'                 DEFAULT: c(NA, 1, 8)
#' @param Range    Ends of color bar. If NULL, it is again the DEFAULT: range(z, finite=TRUE)
#' @param method   Classification method (partial matching is performed),
#'                 see \code{\link{classify}}. DEFAULT: "linear"
#' @param breaks   Specification for method, see \code{\link{classify}}.
#'                 DEFAULT: different defaults for each method
#' @param sdlab    Type of label and breakpoints if \code{method="sd"},
#'                 see \code{\link{classify}}. DEFAULT: 1
#' @param legend   Logical. Should a \code{\link{colPointsLegend}} be drawn? DEFAULT: TRUE
#' @param legargs  List. Arguments passed to \code{\link{colPointsLegend}}.
#'                 DEFAULT: NULL, with some defaults specified internally
#' @param lines    Logical. Should lines be drawn instead of / underneath the points?
#'                 (color of each \code{\link{segments}} is taken from starting point,
#'                 last point is endpoint.) If lines=TRUE and pch is not given,
#'                 pch is set to NA. DEFAULT: FALSE
#' @param nint     Numeric of length 1. Number of interpolation points between each
#'                 coordinate if \code{lines=TRUE}. nint=1 means no interpolation.
#'                 Values below 10 will smooth coordinates and might
#'                 miss the original points. DEFAULT: 30
#' @param xlab,ylab,zlab X axis label, y axis label, \code{\link{colPointsLegend} title}.
#'                 DEFAULT: \code{gsub("\\"", "", deparse(\link{substitute}(x/y/z)))}
#' @param log      Logarithmic axes with log="y", "xy" or "x". 
#'                 For logarithmic colorscale, see method="log".
#'                 DEFAULT: ""
#' @param axes,las Draw axes? Label Axis Style. Only used when add=FALSE.
#'                 See \code{\link{par}}. DEFAULT: axes=TRUE, las=1 (all labels horizontal)
#' @param bglines  If not NULL, passed to \code{\link{abline}} to draw background
#'                 lines before adding colored points. DEFAULT: NULL
#' @param pch      Point CHaracter. See \code{\link{par}}. DEFAULT: 16
#' @param x1,x2,y1,y2 Relative coordinates [0:1] of inset plot, see \code{\link{smallPlot}}.
#'                 Passed to \code{\link{colPointsLegend}}.
#'                 DEFAULT: x: 0.6-0.99, y: 0.88-0.98
#' @param density  Arguments for density line in \code{\link{colPointsLegend}},
#'                 or FALSE to suppress drawing it. DEFAULT: NULL
#' @param horizontal Logical passed to \code{\link{colPointsLegend}}. DEFAULT: TRUE
#' @param quiet    Turn off warnings? DEFAULT: FALSE
#' @param \dots    Further graphical arguments passed to \code{\link{plot}},
#'                 \code{\link{points}} and \code{\link{segments}},
#'                 eg cex, xlim (when add=F), mgp, main, sub, asp (when add=F), etc.
#'                 Note: col does not work, as it is already another argument
#' 
colPoints <- function(
  x, y,
  z,
  data,
  add=TRUE,
  col=seqPal(100),
  col2=c(NA, "grey", "black"),
  Range=range(z, finite=TRUE),
  method="linear",
  breaks=length(col),
  sdlab=1,
  legend=TRUE,
  legargs=NULL,
  lines=FALSE,
  nint=30,
  xlab=gsub("\"", "", deparse(substitute(x))),
  ylab=gsub("\"", "", deparse(substitute(y))),
  zlab=gsub("\"", "", deparse(substitute(z))),
  axes=TRUE,
  log="",
  las=1,
  bglines=NULL,
  pch=16,
  x1=0.6,
  y1=ifelse(horizontal, 0.88, 0.30),
  x2=0.99,
  y2=0.99,
  density=NULL,
  horizontal=TRUE,
  quiet=FALSE,
  ...)
{
 # default labels need to be obtained before x and y are evaluated
xlab <- xlab ; ylab <- ylab ; zlab <- zlab
# error checking:
if(length(nint)>1) if(!quiet) warning("Only the first value of 'nint' is used.")
nint <- nint[1]
if(nint<1) stop("nint must be >= 1, not ",nint,".")
col2 <- rep(col2, length.out=3) # in case only one, two or >3 values are given.
#
# vector vs matrix and dimension check: ----------------------------------------
zmat <- FALSE
# a) argument data is given
if(!missing(data)) # get x, y and z from data.frame
   {
   x <- getColumn(substitute(x), data)
   y <- getColumn(substitute(y), data)
   z <- getColumn(substitute(z), data)
   } else
# b) Regular case: z is a vector
if(is.vector(z))
   {
   if(!(length(x)==length(y) & length(x)==length(z)))
      stop("Vectors x,y,z are not all of the same length! (",length(x),",",length(y),",",length(z),")")
   x <- x ;   y <- y ;   z <- z
   } else
# c) z is a matrix: class(z) = matrix, data.frame, array (2D) - as in image, persp
   {
   zmat <- TRUE
   if(missing(x)) {x <- 1:ncol(z) ; if(missing(xlab)) xlab <- "x" }
   if(missing(y)) {y <- 1:nrow(z) ; if(missing(ylab)) ylab <- "y" }
   if(!(length(x)==ncol(z) & length(y)==nrow(z)))
     stop("Dimension of z (ncol*nrow=",ncol(z),"*",nrow(z),
          ") is not length(x) * length(y) (=",length(x),"*",length(y),")!")
   }
# error checking:
if(diff(range(z, finite=TRUE))==0) if(!quiet) warning("All z-values are equal.")
if(is.null(Range)) Range <- range(z, finite=TRUE)
#
# CLASSIFICATION ---------------------------------------------------------------
if(is.null(col)) col <- seqPal(100) # in case colPoints(x,y,z, if(F)col=divPal(100))
method2 <- method
z2 <- z
atgrey <- NULL
if(zmat && method=="log") {
 method2 <- "linear"
 z2 <- log10(z)
 Range <- log10(Range)
 }
cl <- classify(x=z2, method=method2, breaks=breaks, sdlab=sdlab, Range=Range, quiet=quiet)
output <- cl
output$x <- x
output$y <- y
output$z <- z
# error check:
if(length(col) != cl$nbins) stop("Number of colors (",length(col),
                                 ") is not equal to number of classes (",cl$nbins,").")
#
# ACTUAL PLOTTING --------------------------------------------------------------
if(zmat) 
  {
  dy <- if(length(y)>1 && length(y)==nrow(z)) 0.5*diff(y) else 0
  ylim <- c(max(y,na.rm=TRUE)+dy[length(dy)], min(y,na.rm=TRUE)-dy[1L])
  if(method=="log") 
     {
     lv <- logVals(Range=Range, base=NA, exponent=4)
     lv$sel <- lv$vals >= 10^Range[1] & lv$vals <= 10^Range[2]
     cl$at <-     log10(lv$vals[lv$sel])
     cl$labels <- lv$labs[lv$sel]
     atgrey <- lv$all[lv$all >= 10^Range[1] & lv$all <= 10^Range[2]]
     }
  imargs <- list(x=x,y=y,z=t(z2), col=col, add=add, zlim=Range,
        xlab=xlab, ylab=ylab, las=las, axes=axes, log=log, ...)
  imargs <- owa(list(ylim=ylim), imargs)
  do.call(image, args=imargs)
  lines <- FALSE
  } else
if(!add) plot(x, y, type="n", xlab=xlab, ylab=ylab, las=las, axes=axes, log=log, ...)
if(!is.null(bglines)) do.call(abline, bglines)
# Plot lines if wanted:
if(lines)
  {
  if(missing(pch)) pch <- NA
  # linear interpolation between coordinates (smoother line colors):
  np <- length(x)*nint-nint+1 # replacing NA necessary if NAs are at start or end
  xl <- approx2(x,n=np, quiet=quiet) #approx(replace(x, is.na(x), median(x, na.rm=TRUE)), n=np)$y
  yl <- approx2(y,n=np, quiet=quiet) #approx(replace(y, is.na(y), median(y, na.rm=TRUE)), n=np)$y
  zl <- approx2(z,n=np, quiet=quiet) #approx(replace(z, is.na(z), median(z, na.rm=TRUE)), n=np)$y
  # classify interpolated values:
  cl2 <- classify(x=zl, method=method, breaks=breaks, sdlab=sdlab, Range=Range, quiet=quiet)
  output <- cl
  output$x <- xl
  output$y <- yl
  output$z <- zl
  # Where are NAs in the vectors?
  wNA <- is.na(x) | is.na(y) | is.na(z)
  # change single values (surrounded by NA) to NA:
  for(i in 2:(length(wNA)-1))
      if(isTRUE(wNA[i-1]) & isTRUE(wNA[i+1]))  wNA[i] <- TRUE
  # change interpolated values to NA where corresponding values are NA:
  for(i in which(wNA))
      cl2$index[pmax((i-2)*nint+1, 1) : pmin(i*nint, np)] <- NA
  # Actually draw segments:
  segments(x0=xl[-length(xl)],  y0=yl[-length(yl)],  x1=xl[-1],  y1=yl[-1],
           col=col[cl2$index], ...)
  }
if(!zmat)
  {
  points(x[is.na(z)], y[is.na(z)], col=col2[1], pch=pch, ...)
  points(x, y, col=c(col, col2[2:3])[cl$index], pch=pch, ...)
  }
#
# add legend:
legdefs <- list(z=z2, at=cl$at, labels=cl$labels, bb=cl$bb, nbins=cl$nbins,
                plottriangle=c(cl$below>0,cl$above>0), atgrey=atgrey,
                title=zlab, x1=x1, x2=x2, y1=y1, y2=y2,
                density=density, horizontal=horizontal, tricol=col2[2:3], colors=col)
output <- c(output, legdefs[!names(legdefs) %in% c("nbins","bb","at","labels","index","z")])
if(legend) do.call(colPointsLegend, args=owa(legdefs, legargs))
return(invisible(output))
} # Function end
