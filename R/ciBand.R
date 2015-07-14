# Polygon for confidence interval bands, can handle NA's well
# Berry Boessenkool, berry-b@gmx.de, 2015-07-14

ciBand <- function(
yu,                  # y values of upper confidence region boundary
yl,                  # y values of lower confidence region boundary
ym=NULL,             # y values of median/mean line. Only added if this argument is given
x=1:length(yu),      # x values (one ascending vector)
na="interpolate",    # Method used at NA points. One of "interpolate" or "remove".
nastars=TRUE,        # If na="interpolate", should stars be drawn at places that used to be NA?
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
  if(nastars) points(x[is.na(yu)], yui[is.na(yu)], pch=8)
  if(nastars) points(x[is.na(yl)], yli[is.na(yl)], pch=8)
  # Draw median/mean line:
  if(!is.null(ym)) lines(x, ymi, col=colm)
  }
else if(na=="remove")
  {
  nna <- is.na(yu) | is.na(yl) | is.na(x)
  # break vectors up into sections of consecutive non-NA values:
  sectionstarts <- c(1, which(diff(nna)==-1)+1)
  sectionends   <- c(which(diff(nna)==1), nyl)
  # Actual polygons:
  for(i in 1:length(sectionstarts))
    {
    use <- sectionstarts[i]:sectionends[i]
    yur <- yu[use]
    ylr <- yl[use]
    xr  <-  x[use]
    polygon(x=c(xr, rev(xr)), y=c(yur, rev(ylr)), col=colb, border=border)
    }
  # Draw median/mean line:
  if(!is.null(ym)) lines(x, ym, col=colm)
  # add single points surrounded by NAs:
  # ...
  }
else stop("na method ", na, " is not available (yet).")
}

