#' Seasonality analysis
#'
#' Plot time series to examine it for seasonality
#'
#' @return Data.frame with DOYs of annual maxima and number of nonNA values included
#'         Please Note that the column year does not note the calendrical year 
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
#' Q$discharge[530:540] <- NA
#' plot(Q, type="l")
#' spiralDate(date, discharge, data=Q) # most floods in winter
#' seasonality(date, discharge, data=Q, shift=100, main="NRFA: Thames\nRoyal Windsor Park")
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
#'### @param nmax Number of annual maxima to be marked and returned. DEFAULT: 0
#' @param months Labels for the months. DEFAULT: J,F,M,A,M,J,J,A,S,O,N,D
#' @param xlab,ylab Labels for the axes. DEFAULT: Year, Month
#' @param zlab Title of \code{\link{colPointsLegend}}. DEFAULT: \code{values} name
#' @param ylim,yaxs Parameters specifying y Axis appearance, see \code{\link{par}}. 
#'                  DEFAULT: c(366,1), "i"
#' @param main,adj Graph title and offset to the left 
#'              (\code{adj} passsed to \code{\link{title}}). DEFAULT: "Seasonality", 0.2
#' @param mar,mgp Parameters specifying plot margin size and labels placement.
#'                DEFAULT: c(3,3,4,1), c(2.2,0.7,0)
#' @param keeppar Logical: Keep the margin parameters? If FALSE, they are reset
#'                to the previous values. DEFAULT: TRUE
#' @param \dots Further arguments passed to \code{\link{colPoints}} like 
#'              pch, main, xaxs, but not Range (use \code{vrange})
#'
seasonality <- function(
  dates,
  values,
  data,
  drange=NULL,
  vrange=NULL,
  shift=0,
  janline=TRUE,
###  nmax=0,
  months=substr(month.abb,1,1),
  xlab="Year",
  ylab="Month",
  zlab=substitute(values),
  ylim=c(370,-3),
  yaxs="i",
  main="Seasonality",
  adj=0.2,
  mar=c(3,3,4,1),
  mgp=c(1.9,0.7,0),
  keeppar=TRUE,
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
  # shift break to other month
  if(shift<0) warning("'shift' was negative. Absolute value now used.")
  shift <- abs(shift)
  if(shift>366) stop("'shift' is", shift, ", but should be between 0 and 366.")
  dates <- dates + shift
  # Margin parameters
  op <- par(mar=mar, mgp=mgp)
  if(!keeppar) on.exit(par(op))
  # axis values
  year <- as.numeric(format(dates,"%Y"))
  DOY  <- as.numeric(format(dates,"%j")) # Day of Year
  # Actual plotting
  colPoints(year, DOY, values, Range=vrange, add=FALSE, zlab=zlab,
            ylab=ylab, xlab=xlab, yaxt="n", ylim=ylim, yaxs=yaxs, ...)
  # Axis labelling
  labs <- monthLabs(2004,2004, npm=1) + shift
  lDOY  <- as.numeric(format(labs,"%j"))
  if(janline & shift!=0) abline(h=shift+1)
  axis(2, lDOY, months, las=1)
  title(main=main, adj=adj)
  # Annual maxima
  annmax <- tapply(X=values, INDEX=year, FUN=which.max)# FUN=order, decreasing=TRUE)
  annmax <- data.frame(year=as.numeric(names(annmax)), DOY=annmax)
  rownames(annmax) <- NULL
  annmax$n <- tapply(X=values, INDEX=year, FUN=function(x) sum(!is.na(x)))
  annmax$max <- tapply(X=values, INDEX=year, FUN=max, na.rm=TRUE)
###  lines(annmax, type="o")
  return(annmax)
  }

