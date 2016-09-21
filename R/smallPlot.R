#' Inset small plot within figure
#' 
#' Inset plot with margins, background and border
#' 
#' @return parameters of small plot, invisible.
#' @section Warning: setting mai etc does not work!
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2014
#' @seealso \code{\link{colPointsHist}} for an example of usage, \code{\link[TeachingDemos]{subplot}} and \code{\link[ade4]{add.scatter}} for alternative solutions to this problem that do not set margins.
#' @keywords hplot
#' @importFrom graphics par plot.new rect
#' @export
#' @examples
#' 
#' # Basic usage:
#' op <- par(no.readonly=TRUE) # original parameters
#' plot(1:10)
#' smallPlot(plot(5:1) )
#' smallPlot(plot(5:1), x=c(30,80), y=30:60, bg="yellow", yaxt="n")
#' # if R warns "figure margins too large", try dragging the plot viewer bigger
#' 
#' # select focus for further add-on's:
#' points(2, 2, pch="+", cex=2, col=2) # main window
#' smallPlot( plot(5:1), bg="lightblue", resetfocus=FALSE )
#' points(2, 2, pch="+", cex=2, col=2) # smallPlot window
#' par(op)
#' 
#' # More par settings:
#' plot(1:10)
#' smallPlot( plot(50:1), bg=6, mai=c(0.2, 0.3, 0.1, 0.1)) # screws up
#' smallPlot( plot(5:1), bg=8, ann=FALSE)
#' smallPlot(plot(10:50), bg="transparent") # old plot is kept
#' smallPlot(plot(10:50))
#' 
#' # complex graphics in code chunks:
#' plot(1:10)
#' smallPlot( {plot(5:1, ylab="Blubber"); lines(c(2,4,3));
#'             legend("topright", "BerryRocks!", lwd=3)    }, bg="white" )
#' 
#' 
#' # multiple figure situations
#' old_plt <- par("plt")
#' par(mfcol=c(3,4))
#' new_plt <- par("plt")
#' plot(1:10)
#' plot(1:10)
#' smallPlot(plot(5:1), bg="lightblue", colwise=TRUE)
#' points(3, 2, pch="+", cex=2, col=2)
#' plot(1:10) # canot keep mfcol, only mfrow, if colwise is left FALSE.
#' smallPlot(plot(5:1), bg="bisque", resetfocus=FALSE )
#' points(3, 2, pch="+", cex=2, col=2)
#' plot(1:10) # in smallPlot space
#' par(plt=old_plt)
#' plot(1:10) # too large
#' smallPlot(plot(5:1), bg="palegreen")
#' points(3, 2, pch="+", cex=2, col=2, xpd=NA) # not drawn with default xpd
#' par(plt=new_plt)
#' plot(1:10) # canot keep mfcol, only mfrow, if colwise is left FALSE.
#' smallPlot(plot(5:1), bg="yellow") 
#' points(3, 2, pch="+", cex=2, col=2)   # everything back to normal
#' 
#' par(op)
#' par(mfrow=c(3,4))
#' plot(1:10)
#' plot(1:10)
#' smallPlot(plot(5:1), bg="lightblue", colwise=TRUE)
#' plot(1:10) 
#' smallPlot(plot(5:1), bg="bisque")
#' plot(1:10)
#' 
#' @param expr expression creating a plot. Can be code within {braces}.
#' @param x,y Position of small plot, relative to current figure region (0:100). 
#'        max and min from vector are taken. DEFAULT: 5-70, 50-100
#' @param x1,y1,x2,y2 Positions of topleft and bottomright corner. 
#'        If any is missing, it is taken from x or y
#' @param mar Margin vector in relative units (0:100), thus behaves differently than 
#'        \code{\link{par}(mar)}. DEFAULT: c(12, 14, 3, 3)
#' @param mgp MarGinPlacement: distance of xlab/ylab, numbers and line from plot margin, 
#'        as in \code{\link{par}}, but with different defaults. DEFAULT: c(1.8, 0.8, 0)
#' @param bg Background. DEFAULT: par("bg")
#' @param border Border around inset plot. DEFAULT: par("fg")
#' @param las LabelAxisStyle. DEFAULT: 1
#' @param resetfocus Reset focus to original plot? Specifies where further 
#'        low level plot commands are directed to. DEFAULT: TRUE
#' @param colwise Logical: Continue next plot below current plot? 
#'        If you had \code{par(mfcol=...)}, you must use \code{colwise=TRUE}, 
#'        otherwise the next plot will be to the right of the current plot 
#'        (as with \code{par(mfrow=...)}). DEFAULT: FALSE
#' @param \dots further arguments passed to \code{\link{par}. new=F} removes old plot. 
#'        May mess things up - please tell me for which arguments!
#'  
smallPlot <- function(
expr,
x=c(5,70),
y=c(50,100),
x1,y1,x2,y2,
mar=c(12, 14, 3, 3),
mgp=c(1.8, 0.8, 0),
bg=par("bg"),
border=par("fg"),
las=1,
resetfocus=TRUE,
colwise=FALSE,
...)
{                                            #     ------------
# Input check:                               #  y1 | P1       |
if(missing(x1)) x1 <- min(x, na.rm=TRUE)     #     |          |
if(missing(x2)) x2 <- max(x, na.rm=TRUE)     #  y2 |       P2 |
if(missing(y1)) y1 <- max(y, na.rm=TRUE)     #     ------------
if(missing(y2)) y2 <- min(y, na.rm=TRUE)     #       x1    x2
# catch outside plot:
if(x1<0)  {x1 <- 0;   warning("x (",x1,") set to 0.")}
if(y2<0)  {y2 <- 0;   warning("y (",y2,") set to 0.")}
if(x2>100){x2 <- 100; warning("x (",x2,") set to 100.")}
if(y1>100){y1 <- 100; warning("y (",y1,") set to 100.")}
# correct order of values (only relevant if x1,x2,y1,y2 is given):
if(x2<x1) {temp <- x2;  x2 <- x1;  x1 <- temp;  rm(temp)}
if(y2>y1) {temp <- y2;  y2 <- y1;  y1 <- temp;  rm(temp)}
# control for 0:1 input:
if(  (x1<1 & x2<1) |  (y1<1 & y2<1)  )
   stop("x or y was probably given as coodinates between 0 and 1. They must be between 0 and 100.")
# old parameters to be restored at exit:
op <- par(no.readonly=TRUE)
# inset plot: background, border
par(plt=c(x1, x2, y2, y1)/100, new=TRUE, mgp=mgp) # plt / fig
plot.new() # code line from ade4::add.scatter
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col=bg, border=border)
# inset plot: margins
par(plt=c(x1+mar[2], x2-mar[4], y2+mar[1], y1-mar[3])/100, new=TRUE, las=las, ...)
# Actual plot:
expr
# par of small plot:
sp <- par(no.readonly=TRUE)
# par reset
if(resetfocus)
  {
  par(op)
  if(colwise) par(mfcol=op$mfcol)
  par(mfg=op$mfg) # needed for multiple figure plots
  par(new=op$new)
  }
return(invisible(sp))
}

