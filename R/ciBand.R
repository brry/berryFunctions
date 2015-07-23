# Polygon for confidence interval bands, can handle NA's well
# Berry Boessenkool, berry-b@gmx.de, 2015-07-14

ciBand <- function(
yu,                  # y values of upper confidence region boundary
yl,                  # y values of lower confidence region boundary
ym=NULL,             # y values of median/mean line. Only added if this argument is given
x=1:length(yu),      # x values (one ascending vector)
na="interpolate",    # Method used at NA points. One of "interpolate" or "remove".
nastars=TRUE,        # If na="interpolate", should stars be drawn at places that used to be NA?
singlepoints=TRUE,   # If na="remove", add points for places surrounded by NAs? can be a boolean (T/F) vector of length three for upper, lower, median. Code to identify isolated points is taken from wq::plotTs
args=NULL,           # List of arguments passed to \code{\link{points}} for the previous two arguments
add=FALSE,           # Add to existing plot? If FALSE, plot is called before adding confidence interval
colm="green3",       # Color for median/mean line
colb=addAlpha(colm), # Color of the confidence region band
border=NA,           # \code{\link{polygon}} border
las=1,               # LabelAxisStyle (axis labels turned upright, see \code{\link{par}})
ylim=range(yu,yl, finite=TRUE), # limits of plot
...                  # Further arguments passed to \code{\link{plot}}  - or maybe better polygon??
)
{
# input checking:
# checks to control if inputs can be converted to vectors?
# ...
# recycle singlepoints.
singlepoints <- rep(singlepoints, length.out=3)
# Vector length checking:
nyl <- length(yl)
if( length(yu) != nyl) stop("Vectors yu and yl are not of the same length. (",
                         length(yu), " and ", nyl, " ).")
 if(!is.null(ym)) if( length(ym) != nyl ) stop("Vectors ym and yu/yl are not of the same length. (",
                         length(ym), " and ", nyl, " ).")
if( length( x) != nyl ) stop("Vectors x and yu/yl are not of the same length. (",
                         length( x), " and ", nyl, " ).")
if(!add) plot(x, yu, type="n", las=1, ylim=ylim, ...)
if(na=="interpolate")
  {
  # Interpolations:
  yui <- approx2(yu)
  yli <- approx2(yl)
  x <- approx2(x)
  if(!is.null(ym)) ymi <- approx2(ym)
  # Actual polygon:
  polygon(x=c(x, rev(x)), y=c(yui, rev(yli)), col=colb, border=border)
  # NA stars:
  if(nastars) do.call(points, args=owa(list(x=x[is.na(yu)], y=yui[is.na(yu)], pch=8), args, "x", "y"))
  if(nastars) do.call(points, args=owa(list(x=x[is.na(yl)], y=yli[is.na(yl)], pch=8), args, "x", "y"))
  # Draw median/mean line:
  if(!is.null(ym)) lines(x, ymi, col=colm)
  }
else if(na=="remove")
  {
  nna <- is.na(yu) | is.na(yl) | is.na(x)
  # break vectors up into sections of consecutive non-NA values:
  r <- rle(nna)
  streaks <- cumsum(r$lengths)
  nonna <- which(!r$values)
  sectionstarts <- c(1,streaks+1)[nonna]
  sectionends   <- streaks[nonna]
  #sectionstarts <- c(1, which(diff(nna)==-1)+1)
  #sectionends   <- c(which(diff(nna)==1), nyl)
  if(length(sectionstarts)!=length(sectionends)) stop("non-NA Sections not identified correctly.")
  # Actual polygons:
  for(i in 1:length(sectionstarts))
    {
    use <- sectionstarts[i]:sectionends[i]
    yur <- yu[use]
    ylr <- yl[use]
    xr  <-  x[use]
    polygon(x=c(xr, rev(xr)), y=c(yur, rev(ylr)), col=colb, border=border)
    }
  # Single points (isolated points, surrounded by NA)
  if(any(singlepoints))
    {
    iso <- function(x) # Code taken from wq::plotTs
      {
      x.forward <- c(NA, x[1:(length(x)-1)])
      x.back <- c(x[2:length(x)], NA)
      iso.pts <- is.na(x.forward) & is.na(x.back) & !is.na(x)
      #iso <- ifelse(iso.pts, x, NA)
      iso.pts
      }
    if(singlepoints[1]) do.call(points, args=owa(list(x=x[iso(yu)], y=yu[iso(yu)], 
                                             pch=20, col=colb), args, "x", "y"))
    if(singlepoints[2]) do.call(points, args=owa(list(x=x[iso(yl)], y=yl[iso(yl)], 
                                             pch=20, col=colb), args, "x", "y"))
    }
  # Draw median/mean line:
  if(!is.null(ym)) 
    {
    lines(x, ym, col=colm)
    if(singlepoints[3]) do.call(points, args=owa(list(x=x[iso(yl)], y=yl[iso(yl)], 
                                             pch=20, col=colm), args, "x", "y"))
    }
  } # End of na="remove"
else stop("na method ", na, " is not available (yet).")
}

