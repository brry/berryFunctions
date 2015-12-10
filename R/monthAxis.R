# Function to label date axes
# berry-b@gmx.de, Feb 2015, update labels and midyear Dec 2015
# returns Date object

#midargs=NULL,  # List of arguments passed to \code{\link{axis}} for the year-start lines without labels.

monthAxis <- function(
side=1,        # Which \code{\link{axis}} are to be labeled? (can be several)
startyear=NULL,# Integer. starting year. DEFAULT: NULL = internally computed from \code{\link{par}("usr")}
stopyear=NULL, # Ditto for ending year.
n=5,           # Approximate number of labels that should be printed (as in code{\link{pretty}})
npm=NULL,      # Number of labels per month, overrides n. DEFAULT: NULL = internally computed.
npy=NA,        # Number of labels per year, overrides npm and n.
format="%d.%m.\n%Y", # Format of date, see details in \code{\link{strptime}}
labels=format.Date(d, format), # labels
midyear=FALSE, # Place labels in the middle of the year? if TRUE, format default is "%Y"
mgp=c(3,1.5,0),# MarGinPlacement, see \code{\link{par}}. The second value is for label distance to axis.
cex.axis=1,    # CharacterEXpansion (letter size)
tick=TRUE,     # Draw tick lines?
las=1,         # LabelAxisStyle for orientation of labels. DEFAULT: 1 (upright)
...)           # Further arguments passed to \code{\link{axis}}, like \code{lwd, col.ticks, hadj, lty}, ...
{
# internally needed functions to get Date range from graphic:
getDate <- function(s) as.Date(par("usr")[if(s%%2) 1:2 else 3:4], origin="1970-01-01")
getYear <- function(x) as.numeric(format(x, "%Y"))
# possible combinations of npm, npy:
pos_npm <- c(6, 3, 2, 1, NA,NA,NA,NA,NA)
pos_npy <- c(NA,NA,NA,12, 6, 4, 3, 2, 1)
pos_dif <- c(5,10,15,30,61,91,122,183,365) # number of days between labels
#
# loop around each side:
for(side_i in side)
  {
  # set from and to:
  dateRange <- getDate(side_i)
  startyear_i <- if(missing(startyear)) getYear(dateRange[1]) else startyear
  stopyear_i  <- if(missing(stopyear )) getYear(dateRange[2]) else stopyear
  # determine npm and npy - desired number of days between labels:
  wish_dif <- as.numeric(diff(dateRange)) / n
  # closest value:
  sel <- which.min(abs(pos_dif - wish_dif))
  # select npm and npy from list
  npm_i <- if(is.null(npm)             ) pos_npm[sel] else npm
  npy_i <- if(is.null(npm) & is.na(npy)) pos_npy[sel] else npy
  # calculate dates
  d <- monthLabs(startyear_i, stopyear_i, npm=npm_i, npy=npy_i)
  # Label axis
  if(!midyear) axis(side=side_i, at=d, labels=labels, las=las, mgp=mgp, cex.axis=cex.axis, tick=tick, ...)
  # midyear option:
  if(midyear)
    {
    d <- monthLabs(startyear_i, stopyear_i, npy=2)
    dbor <- d[seq(1,length(d), by=2)] # border dates (=year starting points)
    dmid <- d[seq(2,length(d), by=2)] # mid-year points
    axis(side=side_i, at=dbor, labels=FALSE,  las=las, mgp=mgp, cex.axis=cex.axis, tick=tick,  ...)
    if(missing(format)) format <- "%Y"
    if(missing(mgp)) mgp <- c(3,0.5,0)
    labels <- labels[seq(2,length(d), by=2)]
    axis(side=side_i, at=dmid, labels=labels, las=las, mgp=mgp, cex.axis=cex.axis, tick=FALSE, ...)
    # stop("midyear has not been implemented yet")
    }
  } # End of loop
# output:
return(invisible(d))
} # End of function
