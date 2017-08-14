#' Seasonality analysis
#' 
#' Examine time series for seasonality of high (low) values
#' 
#' @return The output is always invisible, don't forget to assign it.
#'         If returnall=FALSE: Data.frame with \code{year}, \code{n}umber of nonNA entries,
#'         \code{max} value + \code{doy} of annual maxima.
#'         Please note that the column year does not match the calendrical year
#'         if \code{shift!=0}. \cr
#'         if returnall=TRUE: a list with \code{annmax} (df from above) as well as: \cr
#'         \code{data}: data.frame(doy, values, year) and optionally: \cr
#'         \code{plot1, plot3, plot4, plot5}: outputs from \code{\link{colPoints}} \cr
#'         \code{plot2}: output list from \code{\link{spiralDate}} \cr
#'         and other elements depending on plot type, \code{like data3, data4, probs4, width4}.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul-Oct 2016
#' @seealso \code{\link{spiralDate}}, \code{\link{colPoints}},
#'          \url{https://waterdata.usgs.gov/nwis}
#' @keywords aplot
#' @importFrom graphics abline axis par plot segments title
#' @importFrom grDevices extendrange
#' @importFrom stats dnorm
#' @importFrom utils head
#' @export
#' @examples
#' # browseURL("http://nrfa.ceh.ac.uk/data/station/meanflow/39072")
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
#' 
#' # Be careful with your interpretation. This looks normal up to 2007, but then BAM!:
#' seasonality(date, discharge, data=Q[Q$date<as.Date("2007-07-15"),], plot=3, shift=100, nmax=1)
#' seasonality(date, discharge, data=Q[Q$date<as.Date("2007-08-15"),], plot=3, shift=100, nmax=1)
#' 
#' # Shift is important. You don't want to have this event included twice:
#' seasonality(date, discharge, data=Q[850:950,], plot=3, nmax=1, quiet=TRUE, shift=100)
#' 
#' seasonality(date, discharge, data=Q, plot=2) # most floods in winter
#' seasonality(date, discharge, data=Q, plot=5, vlab="Dude, look at annual max Q!")
#' seasonality(date, discharge, data=Q, plot=5, shift=100)
#' s <- seasonality(date, discharge, data=Q, plot=4, shift=100, width=3, returnall=TRUE)
#' str(s, max.lev=1)
#' 
#' \dontrun{ # excluded from CRAN checks because it is slow
#' seasonality(date, discharge, data=Q, plot=3:4, add=0:1, ylim=lim0(400), shift=117)
#' seasonality(date, discharge, data=Q, plot=4, add=TRUE, lwd=3, shift=117, width=3)
#' }
#' 
#' \dontrun{
#' dev.new(noRStudioGD=TRUE, record=TRUE)     # large graph on 2nd monitor
#' par(mfrow=c(2,2))
#' seasonality(date, discharge, data=Q, plot=(1:5)[-4], shift=100)
#' seasonality(date, discharge, data=Q, plot=(1:5)[-4], lwd=2)
#' seasonality(date, discharge, data=Q, plot=(1:5)[-4], nmax=1, shift=100)
#' seasonality(date, discharge, data=Q, plot=(1:5)[-4], col=divPal(100, ryb=TRUE))
#' dev.off()
#' }
#' 
#' @param dates    Dates in ascending order.
#'                 Can be charater strings or \code{\link{strptime}} results,
#'                 as accepted (and coerced) by \code{\link{as.Date}}
#' @param values   Values to be mapped in color with \code{\link{colPoints}}
#' @param data     Optional: data.frame with the column names as given by dates and values
#' @param drange   Optional date range (analogous to xlim), can be a vector like
#'                 \code{dates}. DEFAULT: NA (computed from \code{dates} internally)
#' @param vrange   Optional value range (analogous to ylim), can be a vector like
#'                 \code{values}. DEFAULT: NA (computed from \code{values} internally)
#' @param shift    Number of days to move the year-break to.
#'                 E.g. shift=61 for German hydrological year (Nov to Oct). DEFAULT: 0
#' @param janline  Logical: Should horizontal line be plotted at
#'                 January 1st if \code{shift!=0}? DEFAULT: TRUE
#' @param nmax     Number of annual maxima to be marked, plotted and returned.
#'                 Currently, only 0 and 1 are implemented. DEFAULT: 0
#' @param maxargs  List of arguments passed to \code{\link{lines}} for annual maxima,
#'                 e.g. \code{maxargs=list(type="l", col="red", lty=3)}.
#'                 DEFAULT: NULL (several internal defaults are used, but can be overridden)
#' @param plot     Integer specifying the type of plot.
#'                 Can be a vector to produce several plots. \cr
#'                 0: none, only return the data.frame with annual maxima. \cr
#'                 1: color coded doy (day of the year) over year (the default). \cr
#'                 2: Color coded spiral graph with \code{\link{spiralDate}}. \cr
#'                 3: Spaghetti line plot with discharge over doy, one line per year. \cr
#'                 4: \code{probs} \code{\link{quantileMean}} over doy, with optional
#'                    aggregation window (\code{width}) around each doy. \cr
#'                 5: Annmax over time for crude trend analysis. \cr
#'                 DEFAULT: 1
#' @param add      Logical. Add to existing plot? DEFAULT: FALSE
#' @param nmin     Minimum number of values that must be present per (hydrological)
#'                 year to be plotted in plot type 5. DEFAULT: 100
#' @param probs    Probabilities passed to \code{\link{quantileMean}} for plot=4.
#'                 DEFAULT: c(0,25,50,75,95,99)/100
#' @param width    Numeric: window width for plot=4. Used as sd in gaussian weighting.
#'                 Support (number of values around a DOY passed to
#'                 quantile funtion at least once) is ca 4.9*width.
#'                 The value at doy itself is used 10 times.
#'                 Larger values of width require more computing time.
#'                 DEFAULT: 3
#' @param text     Logical. Call \code{\link{textField}} if plot=4? DEFAULT: TRUE
#' @param texti    Numerical (vector): indices at which to label the lines.
#'                 DEFAULT: seq(200,20,length.out=length(probs))
#' @param textargs List of arguments passed to \code{\link{textField}} for plot=4.
#'                 DEFAULT: NULL
#' @param months   Labels for the months. DEFAULT: J,F,M,A,M,J,J,A,S,O,N,D
#' @param slab,tlab,vlab Labels for the \bold{s}eason, \bold{t}ime (year) and \bold{v}alues
#'                 used on the axes and title of \code{\link{colPointsLegend}}.
#'                 DEFAULT: "Month", "Year", substitute(values)
#' @param xlim,ylim Limits of x and y axis. DEFAULT: NA (specified internally per plot type)
#' @param xaxs,yaxs x and y Axis style, see \code{\link{par}}.
#'                 Use "r" for regular 4\% expansion, "i" for in range only.
#'                 DEFAULT: NA (specified internally per plot type)
#' @param main,adj Graph title and offset to the left
#'                 (\code{adj} passsed to \code{\link{title}}).
#'                 DEFAULT: "Seasonality", 0.2
#' @param mar,mgp  Parameters specifying plot margin size and labels placement.
#'                 DEFAULT: c(3,3,4,1), c(1.7,0.7,0) (Changed for plot 3:5 if not given)
#' @param keeppar  Logical: Keep the margin parameters? If FALSE, they are reset
#'                 to the previous values. DEFAULT: TRUE
#' @param legend   Logical. Should a legend be drawn? DEFAULT: TRUE
#' @param legargs  List of arguments passed as \code{legargs} to \code{\link{colPoints}}.
#'                 DEFAULT: NULL (internally, plots 3 and 5 have density=F as default)
#' @param returnall Logical: return all relevant output as a list instead of only
#'                 annmax data.frame? DEFAULT: FALSE
#' @param quiet    Logical: suppress progress stuff and colPoints messages?
#'                 DEFAULT: FALSE
#' @param \dots    Further arguments passed to \code{\link{colPoints}} like
#'                 pch, main, xaxs, but not Range (use \code{vrange}).
#'                 Passed to \code{\link{spiralDate}} if \code{plot=2},
#'                 like add, format, lines.
#' 
seasonality <- function(
  dates,
  values,
  data,
  drange=NA,
  vrange=NA,
  shift=0,
  janline=TRUE,
  nmax=0,
  maxargs=NULL,
  plot=1,
  add=FALSE,
  nmin=100,
  probs=c(0,25,50,75,95,99.9)/100,
  width=3,
  text=TRUE,
  texti=seq(200,20,length.out=length(probs)),
  textargs=NULL,
  months=substr(month.abb,1,1),
  slab="Month",
  tlab="Year",
  vlab=NA,
  xlim=NA,
  ylim=NA,
  xaxs=NA,
  yaxs=NA,
  main="Seasonality",
  adj=0.2,
  mar=c(3,3,4,1),
  mgp=c(1.7,0.7,0),
  keeppar=TRUE,
  legend=TRUE,
  legargs=NULL,
  returnall=FALSE,
  quiet=FALSE,
  ...
)
{
# Convert before promise 'values' is evaluated:
vlab1 <- if(is.na(vlab)) deparse(substitute(values)) else vlab
allNA <- function(x) all(is.na(x))
# input columns or vectors
if(!missing(data)) # get vectors from data.frame
  {
    dates <- getColumn(substitute(dates),  data)
    values<- getColumn(substitute(values), data)
  }
#check input
if(length(dates)!=length(values)) stop("length of dates and values not equal (",
                                         length(dates),", ",length(values),").")
if(!all(plot %in% 0:5)) stop("The argument 'plot' must be an integer in 0:5, not ", plot)
add <- rep_len(add, length(plot))
add5 <- rep_len(FALSE, 5)
add5[plot] <- add
add <- add5
#
# ### check dates for completeness ###
# convert to date
dates <- as.Date(dates)
# date range (analogous to xlim):
if(!allNA(drange))
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
missingvrange <- allNA(vrange)
vrange <- range(   if(!allNA(vrange)) vrange else values  , na.rm=TRUE)
# shift break to other month
if(shift<0) warning("'shift' was negative (",shift,"). Absolute value now used.")
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
mymax <-      function(xx) if(allNA(xx)) NA else max(xx, na.rm=TRUE)
mywhichmax <- function(xx) if(allNA(xx)) NA else which.max(xx)
annmax$max <- tapply(X=values, INDEX=year, FUN=mymax)
annmax$doy <- tapply(X=values, INDEX=year, FUN=mywhichmax)
annmax$index <- tapply(X=values, INDEX=year, FUN=length)
annmax$index <- c(0,head(cumsum(annmax$index),-1)) + annmax$doy
# correct doy if time series doesn't start at january 1st - shift:
annmax$doy <- as.numeric(format(dates[annmax$index],"%j"))
### nmax for secondary, tertiary, ... maxima. with new function for event separation
#
# output preparation
output <- list(annmax=annmax)
# PLOTTING
#
# Margin parameters
op <- par(mar=mar, mgp=mgp)
if(!keeppar) on.exit(par(op))
# Axis labelling
tick <- monthLabs(2004,2004, npm=1) + shift
labs <- tick + 15
tdoy <- as.numeric(format(tick,"%j"))
ldoy <- as.numeric(format(labs,"%j"))
# Actual plotting
#
xlim1 <- if(allNA(ylim)) extendrange(year, f=0.01) else xlim
xaxs1 <- if(is.na(xaxs)) "i" else xaxs
#
if(1 %in% plot) # doy ~ year, col=Q ----
{
  ylim1 <- if(allNA(ylim)) c(370,-3) else ylim
  yaxs1 <- if(is.na(yaxs)) "i" else yaxs
  output$plot1 <- colPoints(year, doy, values, Range=vrange, add=add[1], yaxt="n",
            xlim=xlim1, ylim=ylim1, xaxs=xaxs1, yaxs=yaxs1, quiet=quiet,
            ylab=slab, xlab=tlab, zlab=vlab1, legend=legend, legargs=legargs, ...)
  # Axis labelling
  if(!add[1]){
  if(janline & shift!=0) abline(h=shift+1)
  axis(2, ldoy, months, tick=FALSE, las=1)
  axis(2, tdoy, labels=FALSE, las=1)
  title(main=main, adj=adj)
  }
  if(nmax==1) do.call(points, owa(list(x=annmax$year, y=annmax$doy,
                                      pch=3, cex=0.5), maxargs, "x","y"))
  if(length(plot)>1) cat(1)
}
#
if(2 %in% plot) # Spiral graph, col=Q ----
{
  output$plot2 <- spd <- spiralDate(dates-shift, values, zlab=vlab1, drange=drange,
             vrange=vrange, months=months, shift=shift, legend=legend,
             legargs=legargs, add=add[2], ...)
  if(!add[2]){
  title(main=main, adj=adj)
  if(janline) segments(x0=0, y0=0, x1=sin(shift/365.25*2*pi), y1=cos(shift/365.25*2*pi))
  }
  if(nmax==1) do.call(points, owa(list(x=spd[annmax$index,"x"], y=spd[annmax$index,"y"],
                                       pch=3, cex=0.3), maxargs, "x","y"))
  if(length(plot)>1) cat(2)
}
# parameters for all next plots
if(any(plot>2))
{
if(missing(mar)) par(mar=c(3,4,4,1))
if(missing(mgp)) mgp <- c(2.7,0.7,0)
xlim3 <- if(allNA(xlim)) c(0,367) else xlim
ylim3 <- if(allNA(ylim)) lim0(vrange) else ylim
xaxs3 <- if(is.na(xaxs)) "i" else xaxs
yaxs3 <- if(is.na(yaxs)) "r" else yaxs
}
#
output$data <- data3 <- data.frame(doy, values, year)
if(3 %in% plot) # Q~doy, col=year ----
{
  # date year range
  if(!exists("drange3", inherits=FALSE)) drange3 <- range(year)
  # NAs between years
  if(diff(range(year, na.rm=TRUE))>0)
    {
    separators <- which(diff(year)!=0)
    separators <- separators + 1:length(separators)
    data3 <- insertRows(output$data, separators)
    output$data3 <- data3
    }
  # plot
  output$plot3 <- colPoints(doy, values, year, data=data3, Range=drange3,
      zlab=tlab, ylab="", xlab=slab, xaxt="n", legend=legend, add=add[3],
      legargs=owa(list(density=FALSE),legargs), quiet=quiet,
      xlim=xlim3, ylim=ylim3, xaxs=xaxs3, yaxs=yaxs3, lines=TRUE, nint=1,
      if(!exists("col", inherits=FALSE)) col=seqPal(100, colors=c("red","blue")),  ...)
  if(!add[3]){
  title(ylab=vlab1, mgp=mgp)
  # Axis labelling
  axis(1, ldoy, months, tick=FALSE, las=1)
  axis(1, tdoy, labels=FALSE, las=1)
  title(main=main, adj=adj)
  if(janline & shift!=0) abline(v=shift+1)
  if(nmax==1) do.call(points, owa(list(x=doy[annmax$index], y=values[annmax$index],
                                  pch=21, cex=0.8, lwd=1.5, bg="white"), maxargs))
  }
  if(length(plot)>1) cat(3)
}
#
if(4 %in% plot) # Qpercentile~doy, col=n ----
{
  # deciding weights:
  if(FALSE){ # kept here for reference only
  width=1; x <- unique(ceiling((-3*width):(3*width)))
  d <- dnorm(x,sd=width) ; plot(x, round(d*10/max(d)) )
  # number of entries used (support) as multiple of width
  xx <- seq(0.6,20,len=1000); plot(xx, sapply(xx, function(width){
      d <- dnorm((-3*width):(3*width), sd=width)
      sum(round(d*10/max(d))>0) / width}), type="l", ylab=""); axis(4,line=-2,las=1)
  if(width< 0) stop("width must be a positive number, not ", width)
  if(width>50) stop("width must be smaller than 50. It is ", width)
  }
  # preparing weights:
  output$width4 <- width
  output$probs4 <- probs
  if(width<1)
  {
  Qp <- lapply(1:366, function(day) quantileMean(values[which(doy==day)],
                                                 probs=probs, na.rm=TRUE)  )
  }
  else
  {
  xx4 <- unique(ceiling(  (-3*width):(3*width)  ))
  w <- dnorm(xx4, sd=width)
  w <- round(w*10/max(w))
  # computing weighted quantile around DOYs
  if(requireNamespace("pbapply", quietly=TRUE) && !quiet) lapply <- pbapply::pblapply
  Qp <- lapply(1:366, function(day)
    {
    select <- base::lapply(which(doy==day), function(i) rep(i+xx4, w) )
    select <- unlist(select)
    select <- select[select>0 & select<length(doy)]
    # output: weighted quantile
    quantileMean(values[select], probs=probs, na.rm=TRUE)
    })
  }
  Qp <- as.matrix(l2df(Qp))
  output$data4 <- Qp
  # plot
  ylim4 <- if(missingvrange) lim0(Qp) else lim0(vrange)
  ylim4 <- if(allNA(ylim)) ylim4 else ylim
  vlab4 <- if(length(probs)==1) paste0(vlab1, " (",probs*100,"th percentile)") else
                                paste0(vlab1, "  percentiles")
  vlab4 <- if(is.na(vlab))vlab4 else vlab
  output$plot3 <- do.call(colPoints, owa(list(x=1:366, y=Qp[,1], z=Qp[,1], add=add[4],
       ylab="", xlab=slab, xaxt="n", legend=FALSE, quiet=quiet, col="blue",
       xlim=xlim3, ylim=ylim4, xaxs=xaxs3, yaxs=yaxs3, lines=TRUE, nint=3), list(...)))
  if(length(probs)!=1)  for(i in 2:length(probs))
     do.call(colPoints, owa(list(x=1:366, y=Qp[,i], z=Qp[,1], add=TRUE, legend=FALSE,
               lines=TRUE, nint=3, col="blue", quiet=quiet), list(...)))
  if(text){
    texti <- rep_len(texti, length(probs))
    texty <- Qp[texti,]
    if(length(probs)!=1) texty <- diag(texty)
    do.call(textField, owa(list(x=texti, y=texty,
                  labels=paste0(round(probs*100,1),"%"), quiet=TRUE), textargs))
    }
  if(!add[4]){
  title(ylab=vlab4, mgp=mgp)
  # Axis labelling
  axis(1, ldoy, months, tick=FALSE, las=1)
  axis(1, tdoy, labels=FALSE, las=1)
  title(main=main, adj=adj)
  if(janline & shift!=0) abline(v=shift+1)
  if(width>=1 & legend) colPointsLegend(rep(xx4,w), colors=NA, title="Smoothing weights",
                              lines=FALSE, ...)
  }
  if(length(plot)>1) cat(4)
}
#
if(5 %in% plot) # annmax~year, col=n ----
{
  vlab5 <- if(is.na(vlab)) paste("annual maximum", vlab1) else vlab
  nalab <- if(shift==0) "n nonNA / year" else "n nonNA / hydrol. year"
  main5 <- if(missing(main)) "Annual maxima" else main
  annmax5 <- annmax
  annmax5[ annmax5$n < nmin , c("n", "max")] <- NA
  ylim5 <- if(allNA(ylim)) range(annmax5$max, na.rm=TRUE) else ylim
  output$plot5 <- colPoints("year", "max", "n", data=annmax5, add=add[5], zlab=nalab,
            xlim=xlim1, xaxs=xaxs1, ylim=ylim5, yaxs=yaxs3, ylab="", xlab=tlab, quiet=quiet,
            legend=legend, legargs=owa(list(density=FALSE),legargs), lines=TRUE, ...)
  if(!add[5]){
  title(ylab=vlab5, mgp=mgp)
  title(main=main5, adj=adj)
  }
  ### nmax once larger values are possible
  if(length(plot)>1) cat(5)
}
#
# Function output
return(invisible(if(returnall) output else annmax))
}

