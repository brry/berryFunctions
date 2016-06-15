#' Spiral graph of time series
#'
#' Plot seasonality of (daily) time series along spiral
#'
#' @return invisible data.frame with date, vals, and the plotting coordinates
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{colPoints}}, \code{\link{as.Date}}
#' @keywords chron hplot aplot color
# @importFrom package fun1 fun2
#' @importFrom graphics plot text
#' @importFrom utils tail
#' @export
#' @examples
#' # synthetic seasonal Data
#' set.seed(42)
#' fakeData <- data.frame(time = as.Date("1985-01-01")+0:5000,
#'                        vals = cumsum(rnorm(5001))+50          )
#' fakeData$vals <- fakeData$vals + sin(0:5000/366*2*pi)*max(abs(fakeData$vals))
#' 
#' sp <- spiralDate(time,vals, data=fakeData)
#' tail(sp)
#' spiralDate(time,vals, data=fakeData, drange=as.Date(c("1980-01-01", "2004-11-15")), lines=TRUE)
#' 
#' par(mfrow=c(1,3), mar=c(3,3,6,1), mgp=c(2,0.6,0), las=1)
#' colPoints(time,vals,vals, data=fakeData, col=divPal(100), add=FALSE, legend=FALSE,
#'           lines=TRUE, pch=NA, nint=1, lwd=2)
#' title(main="classical time series\nworks badly for long time series\nshows trends well")
#'
#' fakeData$Year <- as.numeric(format(fakeData$time,"%Y"))
#' fakeData$DOY  <- as.numeric(format(fakeData$time,"%j")) # Day of Year
#' colPoints(Year, DOY, vals, data=fakeData, add=FALSE, zlab="Daily mean discharge",
#'            ylim=c(366,0), col=divPal(100), legend=FALSE)
#' title(main="yearly time series\nday of year over time\nfails for cyclicity over the winter")
#'
#' spiralDate(time,vals, data=fakeData, col=divPal(100), legargs=list(y1=70,y2=80))
#' title(main="spiral graph\nshows cyclic values nicely 
#'             trends are harder to detect\nrecent values = more visual weight")
#'
#' par(mfrow=c(1,1))
#' 
#' # Data with missing values:
#' fakeData[1300:1500, 2] <- NA
#' spiralDate(time,vals, data=fakeData, lines=TRUE) # no problem
#' # Missing data:
#' fakeData <- na.omit(fakeData)
#' spiralDate(time,vals, data=fakeData, lines=TRUE) # problematic for lines
#' spiralDate(time,vals, data=fakeData, pch=3)      # but not for points
#' 
#' ## Real data:    
#' #library2("waterData")
#' #data(exampleWaterData)
#' #spiralDate(dates, val, data=q05054000LT, lines=TRUE, lwd=3)
#' 
#' @param dates Dates in ascending order. 
#'              Can be charater strings or \code{\link{strptime}} results, 
#'              as accepted (and coerced) by \code{\link{as.Date}}
#' @param values Values to be mapped in color with \code{\link{colPoints}} along seasonal spiral
#' @param data Optional: data.frame with the column names as given by dates and values
#' @param drange Optional date range (analogous to xlim), can be a vector like \code{dates}. DEFAULT: NULL
#' @param vrange Optional value range (analogous to ylim), can be a vector like \code{values}. DEFAULT: NULL
#' @param months Labels for the months
#' @param add Add to existing plot? DEFAULT: FALSE
#' @param prop Proportion of the data to be actually plotted, used in \code{\link{spiralDateAnim}}. DEFAULT: NULL
#' @param zlab Title of \code{\link{colPointsLegend}}
#' @param format Format of date labels see details in \code{\link{strptime}}. DEFAULT: "\%Y"
#' @param nint Number of interpolation segments between points, only used if lines=TRUE. 
#'             DEFAULT: 1 (with long time series, the colPoints default of 30 is too high!)
#' @param \dots Further arguments passed to \code{\link{colPoints}}, but not Range (use \code{vrange})
#'
spiralDate <- function(
dates,
values,
data,
drange=NULL,
vrange=NULL,
months=c("J","F","M","A","M","J","J","A","S","O","N","D"),
add=FALSE, 
prop=NULL,
zlab=substitute(values),
format="%Y",
nint=1,
...
)
{
zlab <- if(missing(zlab)) deparse(zlab) else zlab
#  
if(!missing(data)) # get vectors from data.frame
  {
  dates <- data[ , deparse(substitute(dates))]  
  values<- data[ , deparse(substitute(values))]  
  } 
#check input
if(length(dates)!=length(values)) stop("length of dates and values not equal (",
                                       length(dates),", ",length(values),").")
# convert to date
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
# values range
vrange <- range(   if(!is.null(vrange)) vrange else values  , na.rm=TRUE)  
#
# coordinates for drawing
r <- rescale(as.numeric(dates)) # ascending time-dependent radius
doy <- as.numeric(format(dates, "%j")) # day of year
x <- r*sin(doy/365.25*2*pi)
y <- r*cos(doy/365.25*2*pi)
# output:
out <- data.frame(dates,values,x,y)
# plot selected data only:

if(!is.null(prop))
  {
  sel <- 1:(prop*length(dates))
  x <- x[sel]
  y <- y[sel]
  dates <- dates[sel]
  values <- values[sel]
  }
# plot:
lim <- c(-1.1,1.1)
if(!add) plot(1, xlim=lim, ylim=lim, type="n", ann=FALSE, axes=FALSE, asp=1)
colPoints(x=x,y=y,z=values, Range=vrange, add=TRUE, zlab=zlab, nint=nint, ...)
# labelling months:
f <- 1.1
lx <- f*sin(0:11/12*2*pi)
ly <- f*cos(0:11/12*2*pi)
lsrt <- 12:1/12*360
for(i in 1:12) text(lx[i], ly[i], months[i], srt=lsrt[i])
# labeling years:
text(headtail(x), headtail(y), format(headtail(dates), format))
# output:
return(invisible(out))
}
