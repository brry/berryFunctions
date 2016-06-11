#' arrow at locator point in graph
#'
#' Draw arrow at positions in a graph located by clicking and return the code to recreate it
#'
#' @details Not tested across platforms yet...
#'
#' @return Character string with code
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2016
#' @seealso \code{\link{locLine}}, \code{\link{locator}}, \code{\link{abline}}
#' @keywords aplot iplot
#' @export
#' @examples
#'
#' plot(cumsum(rnorm(60)), type="l")
#' ## locArrow() # only do this manually in interactive() mode
#' ## locArrow(col="blue", lwd=3)
#'
#' @param digits Number of digits coordinates are rounded to with \code{\link{signif}}
#' @param length Length of the edges of the arrow head (in inches). DEFAULT: 0.1
#' @param code Direction of arrow head. DEFAULT: 2 (from first to last point clicked)
#' @param \dots Further arguments passed to \code{\link{arrows}}  like lwd, col etc
#'
locArrow <- function(
digits=2,
length=0.1,
code=2,
...
)
{
# get coordinates:
coord <- locator(n=2)
coord$x <- signif(coord$x, digits)
coord$y <- signif(coord$y, digits)
# concatenate basic arguments into character string:
command <- paste0("arrows(x0=",coord$x[1],", x1=",coord$x[2],
                       ", y0=",coord$y[1],", y1=",coord$y[2],
                       ", length=",length,", code=",code     )
# concatenate further arguments, obeying quotation marks:
arg <- list(...)
sQuote2 <- function(x) if(is.character(x)) paste0("'", x, "'") else x
arg2 <- lapply(arg, sQuote2)
arg3 <- toString( paste(names(arg2), arg2, sep="=") )
if(arg3 != "") arg3 <- paste(",", arg3)
# Put both together:
command <- paste0(command, arg3, ")")
# evaluate and return the command string:
eval(parse(text=command))
command
}
