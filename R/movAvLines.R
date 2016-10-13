#' Moving average with different window widths
#' 
#' Add moving average lines with different window widths to a plot
#' 
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2015
#' @seealso \code{\link{movAv}}, \code{\link{addAlpha}}
#' @keywords ts manip smooth
#' @importFrom graphics lines plot
#' @export
#' @examples
#' 
#' set.seed(42)
#' movAvLines(y=cumsum(rnorm(50)), add=FALSE, lwd=3)
#' 
#' @param x x values of data. DEFAULT: 1:length(y)
#' @param y y values that are smoothed with several window widths
#' @param widths widths of \code{\link{movAv}} windows. DEFAULT: 2:7*2-1
#' @param weights weights within each window
#' @param col color passed to \code{\link{addAlpha}}. DEFAULT: "blue"
#' @param alpha transparency passed to \code{\link{addAlpha}}. DEFAULT: 0.3
#' @param add Logical: Add to existing plot?Set to FALSE to first create
#'        the scatterplot. DEFAULT: TRUE
#' @param las LabelAxisStyle (only relevant if add=FALSE). DEFAULT: 1
#' @param \dots further arguments passed to \code{\link{lines}}
#' 
movAvLines <- function(
x=1:length(y),
y,
widths=c(3,5,7,9,11,13),
weights,
col="blue",
alpha=0.3,
add=TRUE,
las=1,
...
)
{
if(!add) plot(x,y, las=las)
for(i in 1:length(widths))
   lines(x, movAv(y, width=widths[i], weights=weights), col=addAlpha(col, alpha), ...)
}

