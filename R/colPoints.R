# Berry Boessenkool, 2011/12
colPoints <- function(
  x, y, # x,y: Vectors with coordinates of the points to be drawn
  z, # Vector or matrix with accompanying color defining height values
  Range=range(z, finite=TRUE), # Ends of color bar for method=equalinterval
  method="equalinterval", # type of binning or classification method (ways to get color class breakpoints)
  breaks, # specification for method
  sdlab=1, #
  col=rainbow2(cl$nbins), # color palette. default: 100 nuances from blue to red
  col2=par("fg"), # color for points not in the color range (method s or u)
  legargs=NULL, # Arguments for colPointsLegend. FALSE to suppress drawing, TRUE for defaults
  histargs=FALSE, # Arguments for colPointsHist. FALSE to suppress drawing
  add=TRUE, # as in points. add to existing plot? add=F to draw new plot
  lines=FALSE, #  Logical. Should lines be drawn underneath the points?
  nint=30, # Numeric of length 1. Number of interpolation points between each coordinate if lines=TRUE.
  xlab=substitute(x), # axis labels
  ylab=substitute(y),
  las=1, # LabelAxisStyle: all labels horizontally (only relevant when add=FALSE)
  pch=16, # PointCHaracter, see ?par
  ...) # further arguments passed to plot, points and lines, eg cex, xlim (bei add=F), mgp, main, sub, asp (when add=F), etc. NOT col
{
xlab <- xlab ;  ylab <- ylab # defaults need to be set before x and y are evaluated
# error checking:
if(diff(range(z, finite=TRUE)==0)) warning("All z-values are equal.")
# Partial matching of breaks:
PossibleValues <- c("equalinterval", "quantile", "usergiven", "standarddeviation")
method <- PossibleValues[pmatch(method,  PossibleValues)]
#
# vector vs matrix and dimension check: ----------------------------------------
# a) Regular case: z ist a vector
if(is.vector(z))
   {
   if(!(length(x)==length(y) & length(x)==length(z)))
      stop("vectors x,y,z are not all of the same length!")
   x <- x ;   y <- y ;   z <- z
   } else
# b) z is a matrix: class(z) = matrix, data.frame, array (2D) - as in image, persp
   {
   if(missing(x)) {x <- 1:ncol(z) ; xlab <- "x" }
   if(missing(y)) {y <- nrow(z):1 ; ylab <- "y" }
   if(!(length(x)==ncol(z) & length(y)==nrow(z)))
     stop("Dimension of z (ncol*nrow) is not length(x) * length(y)!")
   x <- rep(x, each=nrow(z));  y <- rep(y, ncol(z));  z <- as.vector(z)
   }
#
# CLASSIFICATION # -------------------------------------------------------------
if(method=="equalinterval") if(!missing(col)) breaks <- length(col)
#
cl <- classify(x=z, method=method, breaks=breaks, sdlab=sdlab, Range=Range)
# error check:
if(length(col) != cl$nbins) stop("colPoints: Number of colors is not equal to number of classes.")
#
# ACTUAL PLOTTING --------------------------------------------------------------
if(!add) plot(x, y, col=NA, pch=pch, xlab=xlab, ylab=ylab, las=las, ...)
if(lines)
  {# linear interpolation between coordinates (smoother line colors):
  x2 <- approx(x, n=length(x)*nint)$y
  y2 <- approx(y, n=length(y)*nint)$y
  z2 <- approx(z, n=length(z)*nint)$y
  cl2 <- classify(x=z2, method=method, breaks=breaks, sdlab=sdlab, Range=Range)
  segments(x0=x2[-length(x2)],  y0=y2[-length(y2)],  x1=x2[-1],  y1=y2[-1],
           col=col[cl2$index], ...)
   }
points(x, y, col=col2,          pch=pch, ...)
points(x, y, col=col[cl$index], pch=pch, ...)
#
# add legend:
if(is.list(legargs) | is.null(legargs) | isTRUE(legargs) )
  {
  legdefs <- list(z=z, at=cl$at, labels=cl$labels, bb=cl$bb, nbins=cl$nbins, colors=col)
  do.call(colPointsLegend, args=owa(legdefs, legargs))
  }
#
# add histogramm:
if(is.list(histargs) | is.null(histargs) | isTRUE(histargs) )
  {
  histdefs <- list(z=z, at=cl$at, labels=cl$labels, bb=cl$bb, nbins=cl$nbins, colors=col)
  do.call(colPointsHist, args=owa(histdefs, histargs))
  }
return(invisible(cl))
} # Function end
