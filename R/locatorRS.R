#' @title locator with immediate points in Rstudio
#' @description Have \code{\link{locator}} add points on the graph directly after clicking, even in Rstudio Graphics devices
#' @return List with x and y
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2020
#' @seealso \url{https://stackoverflow.com/q/65147219/1587132}
#' @keywords aplot
#' @export
#' @examples
#' if(interactive()){
#' plot(1:10, type="n")
#' locs <- locator(n=3, type="o") # click on locations in graph. 
#' # If you do not set n at beginning, press ESC to finish
#' locs
#' # In Rstudio, points only appear after finishing.
#' locatorRS(7, col="blue", type="o") # plots after each click
#' }
#' 
#' @param n     Maximum number of points to plot.
#' @param type  As in \code{\link{locator}}, but passed to \code{\link{points}}. 
#'              DEFAULT: "p"
#' @param \dots Further arguments passed to \code{\link{points}}
#'
locatorRS <- function(
n=512, 
type="p", 
...
)
{
on.exit(list(x=x,y=y)) # output even when function is canceled with ESC in console
x <- y <- NULL
for(i in seq_len(n))
  {
  d <- locator(1)
  if(is.null(d)) break # If user pressed ESC in Rstudio Graphics window
  x <- c(x, d$x)
  y <- c(y, d$y)
  points(x,y, type=type, ...)
  }
list(x=x, y=y)
}
