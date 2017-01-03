#' Histogram of logarithmic values
#' 
#' Draw histogram of values on a logarithmic scale with nice axis labels
#' 
#' @return none
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb 2016
#' @seealso \code{\link{logAxis}}, \code{\link{hist}}
#' @keywords aplot dplot
#' @importFrom graphics hist
#' @export
#' @examples
#' 
#' dat <- rbeta(1e4, 2, 18)*100
#' hist(dat, col="tan", breaks=50)
#' logHist(dat)
#' logHist(dat, freq=FALSE)
#' logHist(dat, breaks=50)
#' logHist(dat,xlim=c(0,2)) # xlim in powers of ten
#' logHist(c(-1,0,1,2,2,3,3,4,8,10,50)) # warning for negative values
#' 
#' @param x       Vector of numerical values
#' @param logargs A list of arguments passed to \code{\link{logAxis}}. DEFAULT: NULL
#' @param main    Title of graph, internally from x. DEFAULT: internal name representation
#' @param xlab    X axis label. DEFAULT: internal: name of x
#' @param col     Color of histogram bars
#' @param add     Logical: add to existing plot?
#' @param las     Integer: label axis style. DEFAULT: 1 (numbers upright)
#' @param ylim    2 Numbers: y-axis range. DEFAULT: NULL
#' @param freq    Logical: counts instead of density? DEFAULT: TRUE
#' @param \dots   further arguments passed to \code{\link{hist}} 
#'                like breaks, xlim=c(-1,3), ..., but not xaxt
#' 
logHist <- function(
x,
logargs=NULL,
main=xmain,
xlab=xname,
col="tan",
add=FALSE,
las=1,
ylim=NULL,
freq=TRUE,
...)
{
xname <- deparse(substitute(x))
x <- na.omit(as.numeric(x))
neg <- sum(x<=0)
if(neg>0)
  {
  warning(neg," values <= 0 are discarded (",
          round(neg/length(x)*100,1),"% of ",length(x)," values).")
  x <- x[x>0]
  }
xmain <- paste0("Histogram of log10(",xname,")")
if(is.null(ylim))
  {
  suppressWarnings(h <- hist(log10(x),...,plot=FALSE)) # suppress warnings argument 'xlim' is not made use of
  ylim <- lim0(   if(freq) h$counts else h$density   )
  }
hist(x=log10(x), ..., freq=freq, ylim=ylim, las=las, add=add, main=main, xlab=xlab, xaxt="n")
do.call(logAxis, owa(list(), logargs))
hist(x=log10(x), col=col, ..., freq=freq, add=TRUE)
}

