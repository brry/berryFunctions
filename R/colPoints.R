# Berry Boessenkool, 2011/12, 2014
colPoints <- function(
  x, y, # x,y: Vectors with coordinates of the points to be drawn
  z, # Vector or matrix with accompanying color defining height values
  data, # Optional: data.frame with the column names as given by x,y and z.
  Range=range(z, finite=TRUE), # Ends of color bar for method=equalinterval
  method="equalinterval", # type of binning or classification method (ways to get color class breakpoints)
  breaks, # specification for method
  sdlab=1, #
  col=rainbow2(cl$nbins), # color palette. DEFAULT: 100 nuances from blue to red
  col2=c(NA, 1, 8), # color for z==NA and points not in the color range
  legend=TRUE, # Should a legend be drawn?
  legargs=NULL, # Arguments for colPointsLegend.
  hist=FALSE, # Should a legend be drawn?
  histargs=NULL, # Arguments for colPointsHist. FALSE to suppress drawing
  add=TRUE, # as in points. add to existing plot? add=F to draw new plot
  lines=FALSE, #  Logical. Should lines be drawn underneath the points?
  nint=30, # Numeric of length 1. Number of interpolation points between each coordinate if lines=TRUE.
  xlab=substitute(x), # axis labels
  ylab=substitute(y),
  las=1, # LabelAxisStyle: all labels horizontally (only relevant when add=FALSE)
  pch=16, # PointCHaracter, see ?par
  quiet=FALSE, # Turn off warnings?
  ...) # further arguments passed to plot, points and lines, eg cex, xlim (bei add=F), mgp, main, sub, asp (when add=F), etc. NOT col
{
xlab <- xlab ;  ylab <- ylab # defaults need to be set before x and y are evaluated
# error checking:
if(length(nint)>1) if(!quiet) warning("Only the first value of 'nint' is used.")
nint <- nint[1]
if(nint<1) stop("nint must be >= 1.")
col2 <- rep(col2, length.out=3) # in case only one, two or >3 values are given.
# Partial matching of method:
PossibleValues <- c("equalinterval", "quantile", "logspaced", "standarddeviation", "usergiven")
method <- PossibleValues[pmatch(tolower(method),  PossibleValues)]
if(is.na(method)) stop("method can only be equalinterval, quantile, logspaced, standarddeviation, or usergiven (but the name can be abbreviated).")
#
# vector vs matrix and dimension check: ----------------------------------------
# a) argument data is given
if(!missing(data)) # get x, y and z from data.frame
   {
   x <- data[ , deparse(substitute(x))]  
   y <- data[ , deparse(substitute(y))]  
   z <- data[ , deparse(substitute(z))] 
   } # now continue with case b
# error checking:
if(diff(range(z, finite=TRUE))==0) if(!quiet) warning("All z-values are equal.")
# b) Regular case: z ist a vector
if(is.vector(z))
   {
   if(!(length(x)==length(y) & length(x)==length(z)))
      stop("Vectors x,y,z are not all of the same length!")
   x <- x ;   y <- y ;   z <- z
   } else
# c) z is a matrix: class(z) = matrix, data.frame, array (2D) - as in image, persp
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
cl <- classify(x=z, method=method, breaks=breaks, sdlab=sdlab, Range=Range, quiet=quiet)
# error check:
if(length(col) != cl$nbins) stop("Number of colors is not equal to number of classes.")
#
# ACTUAL PLOTTING --------------------------------------------------------------
if(!add) plot(x, y, col=NA, pch=pch, xlab=xlab, ylab=ylab, las=las, ...)
# Plot lines if wanted:
if(lines)
  {# linear interpolation between coordinates (smoother line colors):
  np <- length(x)*nint-nint+1 # replacing NA necessary if NAs are at start or end
  x2 <- approx(replace(x, is.na(x), median(x, na.rm=TRUE)), n=np)$y
  y2 <- approx(replace(y, is.na(y), median(y, na.rm=TRUE)), n=np)$y
  z2 <- approx(replace(z, is.na(z), median(z, na.rm=TRUE)), n=np)$y
  # classify interpolated values:
  cl2 <- classify(x=z2, method=method, breaks=breaks, sdlab=sdlab, Range=Range, quiet=quiet)
  # Where are NAs in the vectors?
  wNA <- is.na(x) | is.na(y) | is.na(z)
  # change single values (surrounded by NA) to NA:
  for(i in 2:(length(wNA)-1))
      if(isTRUE(wNA[i-1]) & isTRUE(wNA[i+1]))  wNA[i] <- TRUE
  # change interpolated values to NA where corresponding values are NA:
  for(i in which(wNA))
      cl2$index[pmax((i-2)*nint+1, 1) : pmin(i*nint, np)] <- NA
  # Actually draw segments:
  segments(x0=x2[-length(x2)],  y0=y2[-length(y2)],  x1=x2[-1],  y1=y2[-1],
           col=col[cl2$index], ...)
  }
points(x[is.na(z)], y[is.na(z)], col=col2[1], pch=pch, ...)
points(x, y, col=c(col, col2[2:3])[cl$index], pch=pch, ...)
#
# add legend:
if(legend)
  {
  legdefs <- list(z=z, at=cl$at, labels=cl$labels, bb=cl$bb, nbins=cl$nbins,
      colors=col, plottriangle=any(na.omit(cl$index>cl$nbins)), tricol=col2[2:3])
  do.call(colPointsLegend, args=owa(legdefs, legargs))
  }
#
# add histogramm:
if(hist | !missing(histargs))
  {
  histdefs <- list(z=z, at=cl$at, labels=cl$labels, bb=cl$bb, nbins=cl$nbins, colors=col)
  do.call(colPointsHist, args=owa(histdefs, histargs))
  }
return(invisible(cl))
} # Function end
