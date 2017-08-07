#' Label axis with typical running times
#' 
#' Label a numerical axis (in minutes) with time units that are typical for running times (10 sec intervals)
#' 
#' @return List with the positions and labels
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2016
#' @seealso \code{\link{logAxis}}, \code{\link{monthAxis}}
#' @keywords aplot
#' @importFrom graphics axis box
#' @export
#' @examples
#' plot(1:200, xaxt="n")
#' runAxis(t=200, int1=20, int2=10)
#' 
#' @param t Maximum time in minutes
#' @param int1 Primary interval (for labels)
#' @param int2 Secondary interval (for lines)
#' @param side Side of the plot to draw \code{\link{axis}} (1,2,3,4 = bottom, left, top, right)
#' @param linarg List of arguments passed to \code{\link{abline}}
#' @param \dots Further arguments passed to \code{\link{axis}}
#' 
runAxis <- function(
  t=3*60,
  int1=10,
  int2=5,
  side=1,
  linarg=NULL,
  ...
)
{
x1 <- seq(0, max(t,na.rm=TRUE), by=int1)
l1 <- paste0(x1%/%60, ":", formatC(x1%%60, width=2, flag="0"))
x2 <- seq(0, max(t,na.rm=TRUE), by=int2)
l2 <- paste0(x2%/%60, ":", formatC(x2%%60, width=2, flag="0"))
axis(side=side, x1, l1, ...)
if(side%in% c(1,3)) do.call(abline, owa(list(v=x2, col=8), linarg)) else
                    do.call(abline, owa(list(h=x2, col=8), linarg))
box()
return(invisible(list(x1=x1, l1=l1, x2=x2, l2=l2)))
}
