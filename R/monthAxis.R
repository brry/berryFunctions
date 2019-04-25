#' @title Label date axis
#' @description  Labels date axes at sensible monthly intervals in the
#'               time domain of years to decades.
#' @return List with locations of month and year labels and ticks, each a Date vector.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb + Dec 2015, Oct 2017
#' @seealso \code{\link{monthLabs}} for the numbercrunching itself,
#'          \code{\link{timeAxis}} for shorter or longer time frames,
#'          \code{\link{axis.Date}} with defaults that are less nice.
#' @keywords chron aplot dplot
#' @importFrom graphics axis par
#' @importFrom utils str tail
#' @export
#' @examples
#' 
#' set.seed(007) # for reproducibility
#' timePlot <- function(nydays, start="2013-08-25", ...)
#'    plot(as.Date(start)+sort(c(0,sample(1:nydays, 50))),
#'         cumsum(rnorm(51)), type="l", xaxt="n", ann=FALSE, las=1, ...)
#' 
#' timePlot(1100)
#' monthAxis()
#' monthAxis(1, nmonths=6, col.axis="red") # 2013 not labeled anymore
#' monthAxis(side=3, nym_half=2) # if axis > 2 years, label only partially
#' 
#' timePlot(2e3)
#' monthAxis()   # long time series (>nym_none) only have years labeled
#' ma <- monthAxis(side=3, font=2)
#' abline(v=ma$mtics, col=8)
#' abline(v=ma$ytics)          # vertical lines in graph - now add lines/points
#' 
#' 
#' timePlot(900)
#' monthAxis(side=3, mtcl=0) # no tick lines between months
#' monthAxis(ycex=1.4, ytcl=2, lwd.ticks=2)
#' monthAxis(yline=1, col.axis=4, col=4)
#' monthAxis(mcex=1, col.axis="red", yformat=" ") # no years labeled
#' timePlot(900)
#' monthAxis(nmonths=1) # year labeled for short period as well
#' 
#' timePlot(800)
#' monthAxis()
#' monthAxis(mgp=c(2,1,0)) # the same. element 2 is relevant here
#' monthAxis(mgp=c(3,0,0)) # requires change in mline andy yline placement
#' 
#' timePlot(400)
#' ma <- monthAxis(lwd=3, yl=list(col.axis=3), mlabels=letters[1:12], mcex=1)
#' abline(v=ma$mtics, col=8) # use output from monthAxis for other functions
#' 
#' timePlot(80)
#' monthAxis(mlabels=month.abb, mcex=1) # short time series give a warning
#' 
#' timePlot(80, "2013-11-14")
#' monthAxis(mlabels=month.abb, mcex=1, nmonths=0, quiet=TRUE)
#' 
#' # Time axis instead of date axis:
#' plot(as.POSIXct(Sys.time()+c(0,2)*360*24*3600), 1:2, xaxt="n")
#' monthAxis(nmonths=2)
#' 
#' timePlot(800, "2015-01-01")
#' monthAxis()
#' timePlot(900, "2015-01-01", xaxs="i")
#' monthAxis()
#' timePlot(300, "2015-01-01", xaxs="i")
#' monthAxis() # if less than a full year is covered, the year label is centered
#' 
#' @param side     Which \code{\link{axis}} is to be labeled? DEFAULT: 1
#' @param time     Logical indicating whether the axis is \code{\link{POSIXct}},
#'                 not \code{\link{Date}}. DEFAULT: NA, meaning axis value >1e5
#' @param origin   Origin for\code{\link{as.Date}} and \code{\link{as.POSIXct}}.
#'                 DEFAULT: "1970-01-01"
#' @param mlabels  Labels for the months. DEFAULT: J,F,M,A,M,J,J,A,S,O,N,D
#' @param yformat  Format of year labels, see details in \code{\link{strptime}}.
#'                 Use \code{yformat=" "} (with space) to suppress year labeling.
#'                 DEFAULT: "\%Y"
#' @param nmonths  Minimum number of months required before a year at the
#'                 axis boundary is labeled. DEFAULT: 3
#' @param nym_half Number of years on axis above which only every second month
#'                 is labeled. DEFAULT: 3.5
#' @param nym_none Number of years on axis above which the months are
#'                 not labeled. DEFAULT: 5
#' @param mcex     \code{cex.axis} (letter size) for month labels. DEFAULT: 0.7
#' @param ycex     \code{cex.axis} (letter size) for year labels. DEFAULT: 1
#' @param mtcl     Month tick length (negative text line height units).
#'                 0 to suppress ticks. DEFAULT: par("tcl") = -0.5
#' @param ytcl     Year tick length (negative text line height units).
#'                 0 to suppress ticks. DEFAULT: par("tcl")-1.7 = -2.2
#' @param mline    Line of month labels. DEFAULT: -1
#' @param yline    Line of year labels. DEFAULT: 0.2
#' @param las      LabelAxisStyle for orientation of labels. DEFAULT: 1 (upright)
#' @param lrange   Label range (two \code{\link{Date}} values).
#'                 DEFAULT: NA = internally computed from \code{\link{par}("usr")}
#' @param trunc    Vector with two values: Number of days/seconds to truncate
#'                 at the left and right end of lrange. DEFAULT: NA
#' @param ym       Kept for back reference. DEFAULT: TRUE
#' @param mgp      MarGin Placement. Suggested not to change this, since
#'                 _tcl and _line defaults are chosen for the DEFAULT: c(3,1,0)
#' @param mt,ml,yt,yl Lists with further arguments passed to \code{\link{axis}},
#'                 like \code{lwd, col.ticks, lwd.ticks, hadj, lty}, separately
#'                 for month ticks, month labels, year ticks, year labels.
#'                 DEFAULT: NULL
#' @param quiet    Suppress warning about short time axis? DEFAULT: FALSE
#' @param \dots    Arguments passed to \code{\link{axis}} for all 4 elements.
#' 
monthAxis <- function(
side     = 1,
time     = NA,
origin   = "1970-01-01",
mlabels  = substr(month.abb,1,1),
yformat  = "%Y",
nmonths  = 3,
nym_half = 3.5,
nym_none = 5,
mcex     = 0.7,
ycex     = 1,
mtcl     = par("tcl"),
ytcl     = par("tcl")-1.7,
mline    = -1,
yline    = 0.2,
las      = 1,
lrange   = NA,
trunc    = NA,
ym       = TRUE,
mgp      = c(3,1,0),
mt=NULL, ml=NULL, yt=NULL, yl=NULL,
quiet=FALSE,
...
)
{
# Input checks:
op <- par(mgp=mgp)
on.exit(par(op), add=TRUE)
if(yformat=="") warning("yformat='' gives unexpected labeling. Did you mean yformat=' '?")
if(!ym) warning("For ym=FALSE behaviour, use timeAxis (no longer monthAxis).")
if(nmonths>=12) nmonths <- 11
if(length(mlabels)!=12)
 {
 warning("length(mlabels) should be 12, not ",length(mlabels), ". Is now recycled.")
 mlabels <- rep(mlabels, length.out=12)
 }
names(mlabels) <- round0(1:12, pre=2)

# get Date range from current graph or from input:
if(anyNA(lrange))
  {
  lrange <- par("usr")[if(side%%2) 1:2 else 3:4]
  }
else
  {
  # lrange class check:
  if(!inherits(lrange, c("Date","POSIXlt"))) stop("class(lrange) must be 
               'Date' or 'POSIXlt', not '", toString(class(lrange)), "'.")
  lrange <- range(lrange, na.rm=TRUE)
  }
trunc <- rep(trunc, length.out=2) # recycle trunc
if(!is.na(trunc[1])) lrange[1] <- lrange[1] + trunc[1]
if(!is.na(trunc[2])) lrange[2] <- lrange[2] - trunc[2]

# time default (TRUE if values at axis are very large):
if(is.na(time)) time <- lrange[1]>1e5
if(time) lrange <- as.POSIXct(lrange, origin=origin)

lrange <- as.Date(lrange, origin=origin)
dif <- as.numeric(difftime(lrange[2], lrange[1], units="days"))/365.24
if(dif>nym_half) mlabels[1:6*2] <- ""
if(dif>nym_none) mlabels <- ""
if(dif<1 & !quiet) warning("The axis is shorter than a year (", dif*365.24,
                  " days). timeAxis might yield better labels than monthAxis.")

# calculate tick and label locations as date values:
getYear <- function(x) as.numeric(format(x, "%Y"))
# months
mtics <- monthLabs(getYear(lrange[1])-1, getYear(lrange[2])+1, npm=1)
mlabs <- mtics+14
mtics <- mtics[between(mtics, lrange-30, lrange+30, quiet=TRUE)]
mlabs <- mlabs[between(mlabs, lrange, quiet=TRUE)]
# years
ytics <- monthLabs(getYear(lrange[1]), getYear(lrange[2]), npy=1)
ylabs <- ytics[between(ytics, lrange, lrange-365.24, quiet=TRUE)]+183-1
ytics <- ytics[between(ytics, lrange, quiet=TRUE)]

# add year labels for partially displayed years:
jday <- as.numeric(format(lrange,"%j"))
addlow <- between(jday[1],2,365-nmonths*30.5, quiet=TRUE)
addupp <- between(jday[2], nmonths*30.5, 365, quiet=TRUE)
if(addlow & addupp & length(ytics)<1) {ylabs <- mean(lrange)} else
  {
  if(addlow) ylabs <- c(mean(c(lrange[1], ytics[1]), na.rm=T), ylabs)
  if(addupp) ylabs <- c(ylabs, mean(c(lrange[2], tail(ytics,1)), na.rm=T))
  }
ylabs <- ylabs[between(ylabs, lrange, quiet=TRUE)]

# Convert to POSIXct if needed:
if(time)
 {
 mtics <- as.POSIXct(mtics)
 mlabs <- as.POSIXct(mlabs)
 ytics <- as.POSIXct(ytics)
 ylabs <- as.POSIXct(ylabs)
 }

# prepare inputs with defaults:
mtd <- list(side=side, at=mtics, labels=FALSE, tcl=mtcl, ...)
ytd <- list(side=side, at=ytics, labels=FALSE, tcl=ytcl, ...)
mld <- list(side=side, at=mlabs, labels=mlabels[format(mlabs,"%m")],
            las=las, line=mline, cex.axis=mcex, tick=FALSE, ...)
yld <- list(side=side, at=ylabs, labels=format(ylabs,yformat),
            las=las, line=yline, cex.axis=ycex, tick=FALSE, ...)

# actually label axis:
do.call(axis, owa(mtd, mt))
do.call(axis, owa(ytd, yt))
do.call(axis, owa(mld, ml))
do.call(axis, owa(yld, yl))

# output:
return(invisible(list(mlabs=mlabs, ylabs=ylabs, mtics=mtics, ytics=ytics)))
} # End of function
