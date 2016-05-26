#' Spiral graph of time series
#'
#' Plot seasonality of (daily) time serires along spiral
#'
#' @details detailsMayBeRemoved
#'
#' @return invisible data.frame with date, vals, and the plotting coordinates
#' @section Warning: warningMayBeRemoved
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{colPoints}}, \code{\link{as.Date}}
#' @keywords chron
# @importFrom package fun1 fun2
#' @export
#' @examples
#'
#' # fake Data
#' set.seed(42)
#' time <- as.Date("1985-01-01")+0:5000
#' vals <- cumsum(rnorm(5001))
#' vals <- vals + sin(0:5000/366*2*pi)*max(abs(vals))/2
#' 
#' par(mfrow=c(1,2), mar=c(3,3,1,1), mgp=c(2,0.6,0), las=1)
#' colPoints(time,vals,vals, col=divPal(100), add=FALSE, legend=FALSE,
#'           lines=TRUE, pch=NA, nint=1, lwd=2)
#' spiralDate(time,vals, col=divPal(100))
#' spiralDate(time,vals, lines=T, nint=1, pch=NA)
#' 
#' @param dates Dates in ascending order. 
#'              Can be charater strings or \code{\link{strptime}} results, 
#'              as accepted (and coerced) by \code{\link{as.Date}}
#' @param values Values to be mapped in color with \code{\link{colPoints}} along seasonal spiral
#' @param drange Optional data ragne (analogous to xlim), can be a vector like \code{dates}. DEFAULT: NULL
#' @param months Labels for the months
#' @param add Add to existing plot? DEFAULT: FALSE
#' @param zlab Title of \code{\link{colPointsLegend}}
#' @param \dots Further arguments passed to \code{\link{colPoints}}
#'
spiralDate <- function(
dates,
values,
drange=NULL,
months=c("J","F","M","A","M","J","J","A","S","O","N","D"),
add=FALSE, 
zlab=substitute(values),
...
)
{
#check input
if(length(dates)!=length(values)) stop("length of dates and values not equal (",
                                       length(dates),", ",length(values),").")
zlab <- deparse(zlab)
dates <- as.Date(dates)
# date range (analogous to xlim):
if(!is.null(drange))
  {
  dmin <- min(as.Date(drange), na.rm=TRUE)
  dmax <- max(as.Date(drange), na.rm=TRUE)
  dates <- c(dmin, dates, dmax)
  values <- c(NA, values, NA)
  inrange <- dates>=dmin & dates<=dmax
  dates <- dates[inrange]
  values <- values[inrange]
  }
# coordinates for drawing
r <- rescale(as.numeric(dates)) # ascending time-dependent radius
doy <- as.numeric(format(dates, "%j")) # day of year
x <- r*sin(doy/366*2*pi)
y <- r*cos(doy/366*2*pi)
# plot:
lim <- c(-1.1,1.1)
if(!add) plot(1, xlim=lim, ylim=lim, type="n", ann=FALSE, axes=FALSE, asp=1)
colPoints(x=x,y=y,z=values,add=TRUE,zlab=zlab, ...)
# labelling:
f <- 1.1
lx <- f*sin(0:11/12*2*pi)
ly <- f*cos(0:11/12*2*pi)
lsrt <- 12:1/12*360
for(i in 1:12) text(lx[i], ly[i], months[i], srt=lsrt[i])
# output:
return(invisible(data.frame(dates,values,x,y)))
}

if(FALSE){

}
