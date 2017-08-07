#' Distance between points
#' 
#' Calculate distance between points on planar surface
#' 
#' @details The function is quite simple: \code{sqrt((xref - x)^2 + (yref - y)^2)}
#' 
#' @return vector with the distances
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2012
#' @seealso \code{\link[spatstat]{nndist}} in the package \code{spatstat} for distance to nearest neighbour
#' @keywords spatial
#' @export
#' @examples
#' 
#' A <- c(3,  9,-1)
#' B <- c(7, -2, 4)
#' plot(A,B)
#' text(A,B, paste0("P",1:3), adj=1.1)
#' points(3,5, col=2, pch=16)
#' segments(3,5, A,B)
#' distance(A,B, 3,5)
#' text(c(3.2,6,1), c(6,1,4), round(distance(A,B, 3,5),2) )
#' 
#' @param x vector with x-coordinate(s) of point(s)
#' @param y ditto for y
#' @param xref single x coordinate of reference point
#' @param yref ditto for y
#' @param along Logical: Should distances be computed along vector \code{(x,y)}?
#'              If TRUE, \code{(xref,yref)} are ignored.
#'              If both \code{(xref,yref)} are not given, along is set to TRUE.
#' 
distance <- function(
x,
y,
xref,
yref,
along=FALSE
)
{
# input check:
if(missing(xref) & missing(yref)) along <- TRUE
# main function, internally:
dist_int <- function(x,y, xref,yref) sqrt((xref-x)^2 + (yref-y)^2)
if(along)
  c(0,sapply(2:length(x), function(i) dist_int(x[i-1],y[i-1], x[i],y[i])))
else
  dist_int(x,y, xref,yref) # normal case
}
