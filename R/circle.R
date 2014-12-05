# Small helper function drawing circles into existing graphics
# Berry Boessenkool, berry-b@gmx.de, 2012
circle <- function(
  x, # x-coordinate of points, numeric value of length 1
  y, # ditto for y
  r, # radius of the circle, in the graphic's units
  locnum=100, # number of points on circle (more means smoother but slower)
  ...) # Further arguments passed to polygon, like col, border, lwd
{
# input checking - only one circle can be drawn:
if(length(x) >1 | length(y) >1 | length(r) >1 | length(locnum) >1)
  {
  warning("Only the first element of the vectors is used.")
  x <- x[1]; y <- y[1]; r <- r[1]; locnum <- locnum[1]
  }
# input checking - is every value numeric?
if(!is.numeric(x)) stop("x must be numeric")
if(!is.numeric(y)) stop("y must be numeric")
if(!is.numeric(r)) stop("r must be numeric")
# prepare circle line coordinates:
cx <- x+r*cos( seq(0,2*pi,len=locnum) )
cy <- y+r*sin( seq(0,2*pi,len=locnum) )
# actually draw it:
polygon(cx, cy, ...)
}
# Note: if circles look like ellipsis, use plot(... asp=1)
