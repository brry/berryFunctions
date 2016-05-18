#' Evenly spaced points along path
#'
#' Compute waypoints with equal distance to each other along a (curved) path or track given by coordinates
#'
#' @details detailsMayBeRemoved
#' @aliases aliasMayBeRemoved
#'
#' @return Dataframe with the coordinates of the final points
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{distance}}, \code{\link{approx}}
#' @keywords spatial
#' @export
#' @examples
#' x <- c(2.7, 5, 7.8, 10.8, 13.7, 15.8, 17.4, 17.7, 16.2, 15.8, 15.1, 13.1, 9.3, 4.8, 6.8, 12.2)
#' y <- c(2.3, 2.1, 2.6, 3.3, 3.7, 4.7, 7.6, 11.7, 12.4, 12.3, 12.3, 12.3, 12, 12.1, 17.5, 19.6)
#' eP <- equidistPoints(x,y, n=10) ; eP
#' plot(x,y, type="o", pch=4)
#' points(equidistPoints(x,y, n=10), col=4, pch=16)
#' points(equidistPoints(x,y, n=10, nint=1), col=2) # from original point set
#' round(distance(eP$x, eP$y), 2) # the 2.69 instead of 4.50 is in the sharp curve
#' # These points are quidistant along the original track
#' 
#' plot(x,y, type="o", pch=16, col=2)
#' round(sort(distance(x,y)), 2)
#' xn <- equidistPoints(x,y, n=10)$x
#' yn <- equidistPoints(x,y, n=10)$y
#' lines(xn,yn, type="o", pch=16)
#' round(sort(distance(xn,yn)), 2)
#' for(i in 1:8)
#' {
#' xn <- equidistPoints(xn,yn, n=10)$x
#' yn <- equidistPoints(xn,yn, n=10)$y
#' lines(xn,yn, type="o", pch=16)
#' print(round(sort(distance(xn,yn)), 2))
#' } # We may recursively get closer to equidistant along track _and_ air,
#' # but never actually reach it.
#' 
#' # Real dataset:
#' load(system.file("extdata/biketrack.Rdata", package="berryFunctions"))
#' colPoints(lon, lat, ele, data=biketrack, add=FALSE, asp=1, pch=4, lines=TRUE)
#' points(equidistPoints(lon, lat, data=biketrack, n=25), pch=3, lwd=3, col=2)
#' bt2 <- equidistPoints(lon, lat, ele, data=biketrack, n=25)
#' bt2$dist <- distance(bt2$x, bt2$y)*1000
#' colPoints(x, y, z, data=bt2, legend=FALSE)
#' # in curves, crow-distance is shorter sometimes
#' plot(lat~lon, data=biketrack, asp=1, type="l")
#' colPoints(x, y, dist, data=bt2, Range=c(2.5,4), add=TRUE, asp=1, pch=3, lwd=5)
#' lines(lat~lon, data=biketrack)
#'
#' @param x,y,z Vectors with coordinates. z is optional and can be left empty
#' @param data Optional: data.frame with the column names as given by x,y (and z)
#' @param n Number of segments to create along the path (=number of points-1)
#' @param nint Number of points to interpolate between original coordinates (with \code{\link{approx2}}).
#'            Larger numbers give more precisely equidistant points, but increase computing time.
#'            \code{int=1} to not do any interpolation. DEFAULT: 30
#' @param \dots Further arguments passed to \code{\link{approx}}
#'
equidistPoints <- function(
x,
y,
z,
data,
n,
nint=30,
...
)
{
# Start
doz <- !missing(z) # do z interpolation along with x and y?
# data.frame columns:
if(!missing(data)) # get x, y and z from data.frame
   {
           x <- data[ , deparse(substitute(x))]
           y <- data[ , deparse(substitute(y))]
   if(doz) z <- data[ , deparse(substitute(z))]
   }
# input checks coordinates:
x <- as.vector(x)
y <- as.vector(y)
if(doz) z <- as.vector(z)
        if(!is.vector(x) | !is.numeric(x)) stop("x must be a numeric vector.")
        if(!is.vector(y) | !is.numeric(y)) stop("y must be a numeric vector.")
if(doz) if(!is.vector(z) | !is.numeric(z)) stop("z must be a numeric vector.")
lx <- length(x)
        if(lx != length(y)) stop("Lengths of x and y differ (",lx," and ",length(y),").")
if(doz) if(lx != length(z)) stop("Lengths of x and z differ (",lx," and ",length(z),").")
# input checks other arguments
   n <- as.integer(   n[1])
nint <- as.integer(nint[1])
if(   n<1) stop(   "n must be a positive integer.")
if(nint<1) stop("nint must be a positive integer.")
#
# linear interpolation between coordinates:
if(nint>1)
  {
  np <- length(x)*nint-nint+1
          x <- approx2(x, n=np, ...) # replacing NA necessary if NAs are at start or end
          y <- approx2(y, n=np, ...)
  if(doz) z <- approx2(z, n=np, ...)
  }
#
# distances between points:
dist <- distance(x,y)
# points closest to target distances
target_diff <- sum(dist)/n
index <- sapply(0:n, function(i) which.min(abs(cumsum(dist)-target_diff*i)) )
# output:
out <- data.frame(x=x[index], y=y[index])
if(doz) out$z <- z[index]
out
}
