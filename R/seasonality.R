#' Seasonality analysis
#'
#' Plot time series to examine it for seasonality
#'
#' @return Data.frame with \code{year}, \code{n}umber of nonNA entries, 
#'         \code{max} value + \code{doy} of annual maxima.
#'         Please note that the column year does not note the calendrical year 
#'         if \code{shift!=0}. 
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2016
#' @seealso \code{\link{spiralDate}}
#' @keywords aplot
#' @export
#' @examples
#' browseURL("http://nrfa.ceh.ac.uk/data/station/meanflow/39072")
#' qfile <- system.file("extdata/discharge39072.csv", package="berryFunctions")
#' Q <- read.table(qfile, skip=19, header=TRUE, sep=",", fill=TRUE)[,1:2]
#' rm(qfile)
#' colnames(Q) <- c("date","discharge")
#' Q$date <- as.Date(Q$date)
#' Q$discharge[450:581] <- NA
#' plot(Q, type="l")
#' seas <- seasonality(date, discharge, data=Q, shift=100, main="NRFA: Thames\nRoyal Windsor Park")
#' head(seas)
#' # notice how n for nonmissing values is lower in one single hydrological year, 
#' # which includes parts of two consecutive calendarical years.
#' seas <- seasonality(date, discharge, data=Q, plot=2) # most floods in winter
#' seas <- seasonality(date, discharge, data=Q, plot=3)
#' seas <- seasonality(date, discharge, data=Q, plot=3, shift=100)
#' seasonality(date, discharge, data=Q[200:300,], plot=3, nmax=1)
#' seasonality(date, discharge, data=Q[100:200,], plot=3, nmax=1, shift=100)
#' 
#' \dontrun{
#' dev.new(noRStudioGD=TRUE, record=TRUE)     # large graph on 2nd monitor
#' par(mfrow=c(2,2))
#' seas <- seasonality(date, discharge, data=Q, plot=1:4, shift=100)
#' seas <- seasonality(date, discharge, data=Q, plot=1:4, lwd=2)
#' seas <- seasonality(date, discharge, data=Q, plot=1:4, nmax=1, shift=100)
#' seas <- seasonality(date, discharge, data=Q, plot=1:4, col=divPal(100, ryb=TRUE))
#' }
#' 
#' @param dates Dates in ascending order. 
#'              Can be charater strings or \code{\link{strptime}} results, 
#'              as accepted (and coerced) by \code{\link{as.Date}}
#' @param values Values to be mapped in color with \code{\link{colPoints}} 
#' @param data Optional: data.frame with the column names as given by dates and values
#' @param drange Optional date range (analogous to xlim), 
#'               can be a vector like \code{dates}. DEFAULT: NULL
#' @param vrange Optional value range (analogous to ylim), 
#'               can be a vector like \code{values}. DEFAULT: NULL
#' @param shift Number of days to move the year-break to. 
#'              E.g. shift=61 for German hydrological year (Nov to Oct). DEFAULT: 0
#' @param janline Logical: Should horizontal line be plotted at 
#'                January 1st if \code{shift!=0}? DEFAULT: TRUE
#' @param nmax Number of annual maxima to be marked, plotted and returned. 
#'             Currently, only 0 and 1 are implemented. DEFAULT: 0
#' @param maxargs List of arguments passed to \code{\link{lines}} for annual maxima,
#'                e.g. \code{maxargs=list(type="l", col="red", lty=3)}. DEFAULT: NULL
#' @param plot Integer specifying the type of plot. Can be a vector to produce several plots. \cr 
#'             0: none, only data.frame with annual maxima. \cr
#'             1: color coded doy (day of the year) over year (the default). \cr
#'             2: Color coded spiral graph with \code{\link{spiralDate}}. \cr
#'             3: Spaghetti line plot with discharge over doy, one line per year. \cr
#'             4: plot of annmax over time for crude trend analysis. \cr
#'             DEFAULT: 1
#' @param months Labels for the months. DEFAULT: J,F,M,A,M,J,J,A,S,O,N,D
#' @param xlab,ylab,zlab Labels for the axes and title of \code{\link{colPointsLegend}}. 
#'                       Note that these are switched in plot 3 and 4.
#'                       DEFAULT: Year, Month, substitute(values)
#' @param ylim Limits of y axis. DEFAULT: NA (specified internally per plot type)
#' @param xaxs,yaxs x and y Axis style, see \code{\link{par}}. 
#'                DEFAULT: "r" (regular 4\% expansion), "i" (internal range only)
#' @param main,adj Graph title and offset to the left 
#'              (\code{adj} passsed to \code{\link{title}}). DEFAULT: "Seasonality", 0.2
#' @param mar,mgp Parameters specifying plot margin size and labels placement.
#'                DEFAULT: c(3,3,4,1), c(1.7,0.7,0) (Changed for plot 3:4 if not given)
#' @param keeppar Logical: Keep the margin parameters? If FALSE, they are reset
#'                to the previous values. DEFAULT: TRUE
#' @param legargs List of arguments passed as \code{legargs} to \code{\link{colPoints}}.
#'                DEFAULT: NULL (internally, plots 3:4 have density=F as default)
#' @param \dots Further arguments passed to \code{\link{colPoints}} like 
#'              pch, main, xaxs, but not Range (use \code{vrange}).
#'              Passed to \code{\link{spiralDate}} if \code{plot=2}, like add, format, lines.
#'
seasonality <- function(
  dates,
  values,
  data,
  drange=NULL,
  vrange=NULL,
  shift=0,
  janline=TRUE,
  nmax=0,
  maxargs=NULL,
  plot=1,
  months=substr(month.abb,1,1),
  xlab="Year",
  ylab="Month",
  zlab=substitute(values),
  ylim=NA,
  xaxs="r",
  yaxs="i",
  main="Seasonality",
  adj=0.2,
  mar=c(3,3,4,1),
  mgp=c(1.7,0.7,0),
  keeppar=TRUE,
  legargs=NULL,
  ...
)
{
# Convert before promise is evaluated: 
missingzlab <- missing(zlab)
isnaylim <- all(is.na(ylim))
if(missingzlab) zlab <- deparse(zlab)
# input columns or vectors
if(!missing(data)) # get vectors from data.frame
  {
    dates <- getColumn(substitute(dates),  data)
    values<- getColumn(substitute(values), data)
  } 
#check input
if(length(dates)!=length(values)) stop("length of dates and values not equal (",
                                         length(dates),", ",length(values),").")
if(!all(plot %in% 0:4)) stop("The argument 'plot' must be an integer in 0:4, not ", plot)
#
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
  drange3 <- as.numeric(format(c(dmin,dmax), "%Y")) # for plot=3 option
  }
# values range
vrange <- range(   if(!is.null(vrange)) vrange else values  , na.rm=TRUE)  
# shift break to other month
if(shift<0) warning("'shift' was negative. Absolute value now used.")
shift <- abs(shift)
if(shift>366) stop("'shift' is", shift, ", but should be between 0 and 366.")
dates <- dates + shift
# axis values
year <- as.numeric(format(dates,"%Y"))
doy  <- as.numeric(format(dates,"%j")) # Day of Year
#
# COMPUTATION
#
# Annual maxima data.frame
annmax <- tapply(X=values, INDEX=year, FUN=function(x) sum(!is.na(x)))
annmax <- data.frame(year=as.numeric(names(annmax)), n=annmax)
rownames(annmax) <- NULL
annmax$max <- tapply(X=values, INDEX=year, FUN=max, na.rm=TRUE)
annmax$doy <- tapply(X=values, INDEX=year, FUN=which.max)
annmax$index <- tapply(X=values, INDEX=year, FUN=length)
annmax$index <- c(0,head(cumsum(annmax$index),-1)) + annmax$doy
### nmax for secondary, tertiary, ... maxima. with new function for event separation
#
# PLOTTING
#
# Margin parameters
op <- par(mar=mar, mgp=mgp)
if(!keeppar) on.exit(par(op))
# Axis labelling
labs <- monthLabs(2004,2004, npm=1) + shift
ldoy <- as.numeric(format(labs,"%j"))
# Actual plotting
#
if(1 %in% plot) # doy ~ year, col=Q
{
  if(isnaylim) ylim <- c(370,-3)
  colPoints(year, doy, values, Range=vrange, add=FALSE, zlab=zlab,
            ylab=ylab, xlab=xlab, yaxt="n", ylim=ylim, yaxs=yaxs, legargs=legargs, ...)
  # Axis labelling
  if(janline & shift!=0) abline(h=shift+1)
  axis(2, ldoy, months, las=1)
  title(main=main, adj=adj)
  if(nmax==1) do.call(lines, owa(list(x=annmax$year, y=annmax$doy, type="o"), maxargs))
}
#
if(2 %in% plot) # Spiral graph, col=Q
{
  spd <- spiralDate(dates-shift, values, zlab=zlab, drange=drange, vrange=vrange, 
             months=months, shift=shift, legargs=legargs, ...)
  title(main=main, adj=adj)
  if(janline) segments(x0=0, y0=0, x1=sin(shift/365.25*2*pi), y1=cos(shift/365.25*2*pi))
  if(missing(nmax)) nmax <- 0 # looks really ugly
  if(nmax==1) do.call(lines, owa(list(x=spd[annmax$index,"x"], 
                                      y=spd[annmax$index,"y"], type="o"), maxargs))
}
# parameters for both next plots
if(missing(mar)) par(mar=c(3,4,4,1))
if(missing(mgp)) mgp <- c(2.7,0.7,0)
if(isnaylim) ylim <- range(values, na.rm=TRUE)
if(missing(yaxs)) yaxs <- "r"
#
if(3 %in% plot) # Q~doy, col=year
{
  # date year range
  if(!exists("drange3", inherits=FALSE)) drange3 <- range(year)
  xaxs3 <- if(missing(xaxs)) "i" else xaxs
  # NAs between years
  data3 <- data.frame(doy, values, year)
  if(diff(range(year, na.rm=TRUE))>0)
    {
    separators <- which(diff(year)!=0)
    separators <- separators + 1:length(separators)
    data3 <- insertRows(data3, separators)
    }
  # plot
  colPoints(doy, values, year, data=data3, Range=drange3, add=FALSE, zlab=xlab,
            ylab="", xlab=ylab, xaxt="n", legargs=owa(list(density=FALSE),legargs), 
            ylim=ylim, yaxs=yaxs, lines=TRUE, nint=1, xaxs=xaxs3,
            if(!exists("col", inherits=FALSE)) col=seqPal(100, colors=c("red","blue")), 
             ...)
  mtext("") # weird behaviour: title not added without this line. ### Is this a bug?
  title(ylab=zlab, mgp=mgp)
  # Axis labelling
  axis(1, ldoy, months, las=1)
  title(main=main, adj=adj)  
  if(janline & shift!=0) abline(v=shift+1)
  if(nmax==1) do.call(lines, owa(list(x=doy[annmax$index], y=values[annmax$index],
                                    type="p", pch=21, cex=1.5, lwd=2, bg="white"), maxargs))
}
#
if(4 %in% plot) # annmax~year, col=n
{
  if(missingzlab) zlab <- paste("Annual max", zlab)
  colPoints("year", "max", "n", data=annmax, add=FALSE, zlab="n nonNA / hydrol. year",
            ylab="", xlab=xlab, legargs=owa(list(density=FALSE),legargs), lines=TRUE, ...)
  mtext("") ### as above
  title(ylab=zlab, mgp=mgp)
  if(missing(main)) main <- "Trend of annual maxima"
  title(main=main, adj=adj)  
  ### nmax once larger values are possible
}
#
# Function output
return(annmax)
}

