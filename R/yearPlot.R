#' annual plot
#' 
#' Visualize seasonality of time series
#' 
#' @return invisible list with coordinates
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sept 2019
#' @seealso \code{\link{seasonality}}, \code{\link{colPoints}},
#' @keywords aplot
#' @importFrom graphics abline axis par plot segments title
#' @export
#' @examples
#' qfile <- system.file("extdata/discharge39072.csv", package="berryFunctions")
#' Q <- read.table(qfile, skip=19, header=TRUE, sep=",", fill=TRUE)[,1:2]
#' Q$data <- as.Date(Q$data)
#' yearPlot(data, last, data=Q)
#' yearPlot(as.Date(c("2011-06-07","2009-03-25")), 1:2, add=TRUE, pch=3, col=1, legend=FALSE)
#' yearPlot(data, last, data=Q, shift=61)
#' yearPlot(data, last, data=Q, ylim=c(2015,2001))
#' 
#' @param dates    Dates, in any format coerced by \code{\link{as.Date}}.
#' @param values   Values to be mapped in color with \code{\link{colPoints}}
#' @param data     Optional: data.frame from which to use dates and values.
#' @param ylim     (reverse) date range in numerical years. 
#'                 DEFAULT: NULL (computed from \code{dates} internally)
#' @param shift    Number of days to move the year-break to.
#'                 E.g. shift=61 for German hydrological year (Nov to Oct). DEFAULT: 0
#' @param janline  Logical: Should vertical line be plotted at
#'                 January 1st if \code{shift!=0}? DEFAULT: TRUE
#' @param add      Logical. Add to existing plot? DEFAULT: FALSE
#' @param months   Labels for the months. DEFAULT: J,F,M,A,M,J,J,A,S,O,N,D
#' @param xlab,ylab,zlab Axis and legend labels. DEFAULT: ""
#' @param \dots    Further arguments passed to \code{\link{colPoints}} like
#'                 legend, pch, main, xaxs, ...
#' 
yearPlot <- function(
  dates,
  values,
  data,
  ylim=NULL,
  shift=0,
  janline=TRUE,
  add=FALSE,
  months=substr(month.abb,1,1),
  xlab="",
  ylab="",
  zlab="",
  ...
)
{
# input columns or vectors
if(!missing(data)) # get vectors from data.frame
  {
    dates <- getColumn(substitute(dates),  data)
    values<- getColumn(substitute(values), data)
  }
#check input
if(length(dates)!=length(values)) stop("length of dates and values not equal (",
                                         length(dates),", ",length(values),").")
# convert to date
dates <- as.Date(dates)
getyear <- function(d, form="%Y") as.numeric(format(d,form))
if(is.null(ylim)) ylim <- rev(getyear(range(dates,finite=TRUE)))
# shift break to other month
if(shift<0) warning("'shift' was negative (",shift,"). Absolute value now used.")
shift <- abs(shift)
if(shift>366) stop("'shift' is", shift, ", but should be between 0 and 366.")
dates <- dates + shift
# axis values
year <- getyear(dates)
doy  <- getyear(dates, "%j") # Day of Year
#  plotting
output <- colPoints(doy, year, values, add=add, xaxt="n", ylim=ylim,
            ylab=ylab, xlab=xlab, zlab=zlab, ...)
# Axis labelling
if(!add)
  {
  if(shift!=0 & janline) abline(v=shift+1)
  ticks <- c(monthLabs(2004,2004, npm=1),as.Date("2004-12-31"))
  if(shift!=0) ticks <- ticks[1:12] + shift
  axis(1, getyear(ticks, "%j"), labels=FALSE) # tick lines 
  axis(1, getyear(ticks[1:12]+15, "%j"), months, tick=FALSE, las=1, line=-0.5) # labels
  }
# Function output
return(invisible(output))
}

