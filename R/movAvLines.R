#' Moving average with different window widths
#' 
#' Add moving average lines with different window widths to a plot
#' 
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2015
#' @seealso \code{\link{movAv}}, \code{\link{addAlpha}}
#' @keywords ts manip smooth
#' @export
#' @examples
#' 
#' set.seed(42)
#' movAvLines(cumsum(rnorm(50)), plot=TRUE, lwd=3)
#' 
#' @param y y values that are smoothed with several window widths
#' @param x x values of data. DEFAULT: 1:length(y)
#' @param widths widths of \code{\link{movAv}} windows. DEFAULT: 2:7*2-1
#' @param weights weights within each window
#' @param col color passed to \code{\link{addAlpha}}. DEFAULT: "blue"
#' @param alpha transparency passed to \code{\link{addAlpha}}. DEFAULT: 0.3
#' @param plot should scatterplot be created first? DEFAULT: FALSE
#' @param las LabelAxisStyle (only relevant if plot=TRUE). DEFAULT: 1
#' @param \dots further arguments passed to \code{\link{lines}}
#' 
movAvLines <- function(
y,             # y values that are smoothed with several window widths
x=1:length(y), # x values of data
widths=2:7*2-1,# widths of \code{\link{movAv}} windows
weights,       # weights within each window
col="blue",    # color passed to \code{\link{addAlpha}}
alpha=0.3,     # transparency passed to \code{\link{addAlpha}}
plot=FALSE,    # should scatterplot be created first?
las=1,         # LabelAxisStyle (only relevant if plot=TRUE)
...            # further arguments passed to \code{\link{lines}}
)
{
if(plot) plot(x,y, las=las)
for(i in 1:length(widths))
   lines(x, movAv(y, width=widths[i], weights=weights), col=addAlpha(col, alpha), ...)
}

