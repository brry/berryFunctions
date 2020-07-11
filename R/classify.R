#' Classification into groups
#' 
#' classify continuous values into categories with different methods:\cr
#' - linearly or logarithmically spaced equal intervals,\cr
#' - intervals based on quantiles (equally filled bins),\cr
#' - intervals based on  distance from the mean in normal distributions,\cr
#' - user specified class borders (e.g. for legal or critical limits).
#' 
#' @details
#' Binning methods are explained very nicely in the link in the section References.\cr
#' \emph{nbins} indicates the number of classes (and thus, colors).\cr \cr
#' 
#' \tabular{llll}{
#' \bold{\code{method}} \tab |  explanation                   \tab |  meaning of \code{breaks}                    \tab |  default \cr
#' ----------     \tab |  -----------                         \tab |  -----------                                 \tab |  ------- \cr
#' \bold{linear}  \tab |  \emph{nbins} equally spaced classes \tab |  \emph{nbins}                                \tab |  100     \cr
#' \bold{log}     \tab |  \emph{nbins} logarithmically spaced \tab |  \emph{nbins}                                \tab |  100     \cr
#' \bold{quantile}\tab |  classes have equal number of values \tab |  the quantiles (or number of them)           \tab |  0:4/4   \cr
#' \bold{sd}      \tab |  normal distributions                \tab |  number of sd in one direction from the mean \tab |  3       \cr
#' \bold{custom}  \tab |  user-given breakpoints              \tab |  breakpoint values (including ends of Range) \tab |  none    \cr
#' }
#' The default is set to equalinterval which makes sense for my original intent
#' of plotting lake depth (bathymetry measured at irregularly distributed points) on a linear color scale.\cr
#' This is the workhorse for \code{\link{colPoints}}.\cr
#' 
#' @return if \code{col=NULL}, a list with class numbers (index) and other 
#'         elements for \code{\link{colPoints}}. If \rcode{col} is a palette function,
#'         a vector of colors.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2014
#' @seealso \code{\link{colPoints}}
#' @references See this page on the effect of classification (binning) methods: \cr
#' \url{http://uxblog.idvsolutions.com/2011/10/telling-truth.html}
#' @keywords classif
#' @importFrom stats quantile sd
#' @export
#' @examples
#' 
#' classify( c(1:10, 20), "lin", breaks=12)
#' classify( c(1:10, 20), "q", breaks=0:10/10)
#' classify( c(1:10, 20), "s", sdlab=2 )
#' classify( c(1:10, 20), "s", sdlab=1, breaks=2 )
#' classify( c(1:10, 20), "c", breaks=c(5,27) )
#' classify( c(1:10, 20), "log")
#' 
#' cols <- classify( c(1:10, 20), col=seqPal) ; cols
#' plot(c(1:10, 20), col=cols, pch=16, cex=2)
#' 
#' set.seed(42); rz <- rnorm(30, mean=350, sd=120)
#' plot(1)
#' classleg <- function(method="linear", breaks=100, sdlab=1, logbase=1, ...)
#'            do.call(colPointsLegend, owa(
#'            classify(rz, method=method, breaks=breaks, sdlab=sdlab, logbase=logbase),
#'            list(z=rz, title="", ...))   )
#' classleg(br=3, met="s", col=divPal(5),mar=c(0,3,1,0),hor=FALSE,x1=0.1,x2=0.25)
#' classleg(br=3, met="s", col=divPal(6),mar=c(0,3,1,0),hor=FALSE,x1=0.25,x2=0.4, sdlab=2)
#' classleg(y1=0.85, y2=1)
#' classleg(br=20, met="log", y1=0.70, y2=0.85)
#' classleg(br=20, met="log", y1=0.55, y2=0.70, logbase=1.15)
#' classleg(br=20, met="log", y1=0.45, y2=0.60, logbase=0.90)
#' classleg(br= 5, met="q", y1=0.30, y2=0.45)# quantiles: each color is equally often used
#' classleg(met="q", y1=0.15, y2=0.30, breaks=0:15/15, at=pretty2(rz), labels=pretty2(rz) )
#' 
#' @param x      Vector with numeric values
#' @param method Character string (partial matching is performed).
#'               Classification method (type of binning) to compute the
#'               class breakpoints. See section Details. DEFAULT: "linear"
#' @param breaks Specification for method, see Details.
#'               DEFAULT: NULL (different defaults for each method)
#' @param Range  Ends of intervals. DEFAULT: range(x, finite=TRUE)
#' @param col    Function that will return a color palette, e.g. \code{\link{seqPal}}.
#'               If given, a vector of colors is returned instead of the regular list.
#'               DEFAULT: NULL (ignored)
#' @param sdlab  Type of label and breakpoints if \code{method=standarddeviation}.
#'               1 means \code{-0.5 sd, 0.5 sd}, 2 means \code{-1 sd, mean, 1 sd},
#'               3 means actual numbers for type 1, 4 means numbers for type 2.
#'               DEFAULT: 1
#' @param logbase base for \code{\link{logSpaced}}. Used only if not 1 and method="log".
#'               DEFAULT: 1
#' @param quiet  Suppress warnings, eg for values outside Range? DEFAULT: FALSE
#' @param \dots  Further arguments passed to the function \code{col}.
#'
classify <- function(
  x,
  method="linear",
  breaks=NULL,
  Range=range(x, finite=TRUE),
  col=NULL,
  sdlab=1,
  logbase=1,
  quiet=FALSE,
  ...)
{
# input checks -----------------------------------------------------------------
x <- as.numeric(x)
# error checking:
if(length(Range) != 2)
  {
  if(!quiet) warning("Range had ",length(Range)," values, range(Range) is now used.")
  Range <- range(Range, finite=TRUE)
  }
if(diff(Range)==0)
   {
   if(!quiet) warning("The Range values were equal (",Range[1],"). Range is now extended.")
   Range[1] <- Range[1] -1
   Range[2] <- Range[2] +1
   }
# Partial matching of method:
PossibleMethods <- c("linear", "log", "quantile", "sd", "custom")
method0 <- method
method <- PossibleMethods[pmatch(tolower(method),  PossibleMethods)]
if(is.na(method)) stop("method can not be '",method0,"'. Must be one of ",
                       toString(PossibleMethods), " (may be abbreviated).")
# actual work:
if(method=="linear") # linear --------------------------------------------------
{
if(is.null(breaks)) breaks <- 100     # default for breaks
if(length(breaks)!=1) stop("breaks must be a single value if method='linear'. ",
                           "Length is: ", length(breaks))
nb <- breaks                          # number of bins (classes)
bb <- seqR(Range, length.out=nb+1)    # bin borders
at <- pretty2(bb)                     # position of labels in colPointsLegend / -Hist
la <- at                              # labels -"-
ix <- cut(x, breaks=bb, labels=FALSE, include.lowest=TRUE) # index of class for each value of x
} else if(method=="log") # log -------------------------------------------------
{
if(is.null(breaks)) breaks <- 100
if(length(breaks)!=1) stop("breaks must be a single value if method='log'. ",
                           "Length is: ", length(breaks))
if(any(Range<=0)) stop("Range must be postive if method='log'. It is: ", toString(Range))
# breaks and labels:
if(almost.equal(logbase, 1))
{
nb <- breaks
bb <- 10^seq(from=log10(0.999*Range[1]), to=log10(1.001*Range[2]), length.out=nb)
logv <- logVals(Range=log10(Range), base=NA)
at <- logv$vals[between(logv$vals, Range[1], Range[2])]
la <- logv$labs[between(logv$vals, Range[1], Range[2])]
} else {
bb <- logSpaced(base=logbase, n=breaks, min=0.999*Range[1], max=1.001*Range[2], plot=FALSE)
bb <- signif(bb, 5) # else min is in reality min + 1e-13
bb <- unique(bb) # as logspaced produces a lot of breaks in one region
nb <- length(bb)
at <- pretty2(bb)
la <- at
}
ix <- cut(x, breaks=bb, labels=FALSE, include.lowest=TRUE)
} else if(method=="quantile") # quantile ---------------------------------------
{
if(is.null(breaks)) breaks <- 0:4/4
if(length(breaks)==1) breaks <- seq(0,1, length.out=breaks+1)
if(any(breaks<0 | breaks>1)) stop("breaks must be between 0 and 1 if method='quantile'.")
bb <- unique(quantile(x, probs=breaks, na.rm=TRUE))
nb <- length(bb) - 1
at <- bb
la <- signif(bb, 2) # rounding
ix <- cut(x, breaks=bb, labels=FALSE, include.lowest=TRUE)
} else if(method=="sd") # sd ---------------------------------------------------
{
if(is.null(breaks)) breaks <- 3
breaks <- as.integer(breaks)
if(length(breaks)>1) {breaks <- breaks[1]; if(!quiet) warning("breaks was vector. Only first element is used.")}
if(is.na(breaks)) stop("breaks must be an integer")
if(breaks <0) stop("breaks must be a positive integer >=1 if method='sd'.")
if(sdlab==2|sdlab==4)
  {
  nb <- 2*breaks
  bb <- mean(x) + (-breaks:breaks)*sd(x)
  }
else
  {
  nb <- 2*breaks - 1
  bb <- mean(x) + (-breaks:(breaks-1)+0.5)*sd(x)
  }
at <- bb
if(sdlab==2)           {la <- paste(-breaks:breaks, "sd"); la[breaks+1] <- "m"} else
if(sdlab==3 | sdlab==4) la <- signif(at, 2) else
                        la <- paste(-breaks:(breaks-1)+0.5, "sd")
ix <- cut(x, breaks=bb, labels=FALSE, include.lowest=TRUE)
} else if(method=="custom") # custom -------------------------------------------
{
if(is.null(breaks)) stop("breaks _must_ be specified if method is 'custom'.")
if(length(breaks)==1) {breaks <- c(min(x,na.rm=TRUE), breaks, max(x,na.rm=TRUE))
                       if(!quiet) warning("breaks were expanded by range (x).")}
nb <- length(breaks) - 1
bb <- breaks
at <- bb
la <- signif(breaks, 2)
ix <- cut(x, breaks=bb, labels=FALSE, include.lowest=TRUE)
} else
stop("method '",method,"', went wrong internally. Please report via berry-b@gmx.de")
# Range Warning:
###
below <- above <- 0
if(anyNA(ix))
  {
  below <- x < min(bb, na.rm=TRUE)
  above <- x > max(bb, na.rm=TRUE)
  ix[below] <- nb+1
  ix[above] <- nb+2
  below <- sum(below, na.rm=TRUE)
  above <- sum(above, na.rm=TRUE)
  }
if(min(bb,na.rm=TRUE) > min(x,na.rm=TRUE) | max(bb,na.rm=TRUE) < max(x,na.rm=TRUE) )
  if(!quiet) warning("There are ", sum(ix>nb), " (out of ",length(ix),") ",
                     "values outside of the range of the given classes.\n",
                     "These are given the index ", nb+1, " (lower, n=",below,
                     ") and ", nb+2, " (higher, n=",above,").")
# Results
if(!is.null(col))
 {
 if(!is.function(col)) stop("col must be a function returning a palette, not a", 
                            toString(class(col)))
 colors <- col(...)
 nlarge <- sum(ix>length(colors))
 if(nlarge>0) warning("There are ", nlarge, " indexes greater than length of colors (",
                      length(colors),").")
 return(colors[ix])
 }
list(nbins=nb, bb=bb, below=below, above=above, at=at, labels=la, index=ix)
} # Function end
