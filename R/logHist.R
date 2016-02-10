# histogram on logarithmic x axis

logHist <- function(
x, # Vector of numerical values
logargs=NULL, # A list of arguments passed to \code{\link{logAxis}}
main=xmain, # Title of graph, internally from x
xlab=xname, # X axis label.
...) # further arguments passed to \code{\link{hist}} like breaks, col, ..., but not xaxt or add.
{
xname <- deparse(substitute(x))
xmain <- paste0("Histogram of log10(",xname,")")
hist(x=log10(x), ..., main=main, xlab=xlab, xaxt="n")
do.call(logAxis, owa(list(), logargs))
hist(x=log10(x), ..., add=TRUE)
}

