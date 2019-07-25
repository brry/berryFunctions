#' Draw circle with a given radius
#' 
#' Draws a filled circle with a certain radius (in existing plot's units) using \code{\link{polygon}} and \code{\link{sin}}
#' 
#' @note If circles look like ellipsis, use plot(... asp=1)
#' @return data.frame of coordinates, invisible
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2012
#' @seealso \code{\link{symbols}}, \code{\link{polygon}}
#' @keywords aplot
#' @importFrom grDevices rgb
#' @importFrom graphics polygon
#' @export
#' @examples
#' 
#' plot(1:20, type="n", asp=1)
#' circle(5,5, r=3)   # 1:1 aspect shows they're really circles and not ellipses.
#' circle(15,10, r=4, locnum=12, col=2, border=4, lwd=3)
#' 
#' # can not be vectorized:
#' x <- sample(1:20, 15) ;  y <- sample(1:20, 15) ; r <- runif(20)*3
#' circle(x,y,r, col=rgb(1,0.5,0,alpha=0.4), border=NA)
#' for(i in 1:15) circle(x[i],y[i],r[i], col=rgb(1,0.5,0,alpha=0.4), border=NA)
#' 
#' @param x x coordinate of points, numeric value of length 1
#' @param y y coordinate
#' @param r radius of the circle in units of current plot.
#'          Can have two values for an ellipse.
#' @param locnum number of calculated points on the circle (more means smoother but slower). DEFAULT: 100
#' @param \dots further arguments passed to \code{\link{polygon}}, like col, border, lwd
#' 
circle <- function(
  x,
  y,
  r,
  locnum=100,
  ...)
{
# input checking - only one circle can be drawn:
checkinput <- function(i, maxlen=1)
 {
 n <- deparse(substitute(i))
 if(!is.numeric(i)) stop(n, " must be numeric")
 if(length(i) > maxlen) warning("Only the first element of ",n," is used.", call.=FALSE)
 i <- rep(i, length.out=maxlen)
 i[1:maxlen]
 }
x      <- checkinput(x)
y      <- checkinput(y)
r      <- checkinput(r, 2)
locnum <- checkinput(locnum)
# prepare circle line coordinates:
cx <- x+r[1]*cos( seq(0,2*pi,len=locnum) )
cy <- y+r[2]*sin( seq(0,2*pi,len=locnum) )
# actually draw it:
polygon(cx, cy, ...)
return(invisible(data.frame(x=cx, y=cy)))
}
