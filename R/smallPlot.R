#' Inset small plot within figure
#' 
#' multipanel-compatible inset plot with margins, background and border
#' Adding points after \code{smallPlot} is called may be
#' incorrect if the original function messes with the graph margins,
#' see the note in \code{\link{colPointsLegend}}.
#' 
#' @return parameters of small plot, invisible.
#' @section Warning: setting mai etc does not work!
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2014-2016
#' @seealso \code{\link{colPointsLegend}} for an example of usage.
#'          \code{subplot} in the archived \href{https://cran.r-project.org/package=TeachingDemos}{TeachingDemos} package and \code{\link[ade4]{add.scatter}}
#'          for alternative solutions to this problem that do not set margins.
#' @keywords hplot dplot aplot
#' @importFrom graphics par plot.new rect strheight strwidth text
#' @export
#' @examples
#' 
#' # Basic usage:
#' op <- par(no.readonly=TRUE) # original parameters
#' plot(1:10)
#' smallPlot(plot(5:1, ylab="Yo man!"), bg="lightgreen" )
#' smallPlot(plot(5:1), x1=0.5,x2=1, y1=0.3,y2=0.6, bg="yellow", yaxt="n")
#' # if R warns "figure margins too large", try dragging the plot viewer bigger
#' 
#' # select focus for further add-on's:
#' points(2, 2, pch="+", cex=2, col=2) # main window
#' smallPlot( plot(5:1), bg="lightblue", resetfocus=FALSE )
#' mtext("dude")
#' points(2, 2, pch="+", cex=2, col=2) # smallPlot window
#' par(op)
#' 
#' # More par settings:
#' plot(1:10)
#' smallPlot( plot(50:1), bg=6, mai=c(0.2, 0.3, 0.1, 0.1)) # screws up
#' smallPlot( plot(5:1), bg=8, ann=FALSE)
#' smallPlot(plot(10:50), bg="transparent") # old plot is kept
#' 
#' # complex graphics in code chunks:
#' plot(1:100)
#' smallPlot( {plot(5:1, ylab="Rocky label"); lines(c(2,4,3));
#'             legend("topright", "BerryRocks!", lwd=3)    }, bg="white")
#' 
#' 
#' # multiple figure situations
#' par(op)
#' par(mfcol=c(3,4))
#' plot(1:10)
#' plot(1:10)
#' smallPlot(plot(5:1), bg="lightblue")
#' plot(1:10)
#' smallPlot(plot(5:1), bg="bisque", colwise=TRUE) # if mfcol (not mfrow) was set
#' plot(1:10)
#' 
#' # Outer margins (e.g. to add legends to multi-panel plots)
#' par(op)
#' par(mfrow=c(3,2), oma=c(0,5,0,0), mar=c(0,0,1,0)+0.5)
#' for(i in 0:5*4) image(volcano+i, zlim=c(90,200), xaxt="n", yaxt="n",
#'                       main=paste("volcano +", i))
#' smallPlot(plot(1:10), x1=0,x2=0.25, y1=0.5,y2=1, bg="green", mar=1)
#' smallPlot(plot(1:10), x1=0,x2=0.25, y1=0.5,y2=1, bg="green", mar=1, outer=TRUE)
#' colPointsLegend(90:200, horizontal=FALSE, x1=0, col=heat.colors(12), outer=TRUE,
#'                labelpos=5, density=FALSE, title="", cex=2, lines=FALSE)
#' 
#' 
#' # Further testing with mfrow and mfcol
#' par(op)
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
#' par(op)
#' 
#' # if layout is used instead of par(mfrow), it is difficult to add graphs
#' # after using smallPlot
#' lay <- matrix(c(1,1,1,1,2,2,3,3,2,2,3,3,4,4,5,5), ncol=4)
#' layout.show(layout(lay))
#' layout(lay)
#' plot(1:10)
#' plot(1:10)
#' smallPlot(plot(1:10), mar=c(1,3,1,0), x1=0,x2=0.2, y1=0.2,y2=0.8, bg=4, outer=TRUE)
#' # plot(1:10) # now in a weird location (par("mfrow") is 4x4 after layout)
#' 
#' @param expr expression creating a plot. Can be code within curly braces.
#' @param x1,x2,y1,y2 Position of small plot, relative to current figure region [0:1].
#'                    DEFAULT: x: 0.05-0.7, y: 0.5-1
#' @param outer Logical. Should inset plot be placed in the device outer margin region
#'              instead of relative to the current figure region?
#'              Useful in multipanel plots with par(oma). \code{outer} here does not
#'              have exactly the same meaning as in \code{\link{title}}. DEFAULT: FALSE
#' @param xpd Plotting and notation clipped to plot region (if xpd=FALSE),
#'            figure region (TRUE) or device region (xpd=NA). DEFAULT: NA
#' @param mar Margin vector in (approximate) number of lines. It is internally
#'            multiplied with \code{\link{strheight}} to convert it to relative units [0:1],
#'            thus the behaviour is a bit different from \code{\link{par}(mar)}.
#'            It's recycled, so you can use \code{mar=0}. DEFAULT: c(3,3,1,1)
#' @param mgp MarGinPlacement: distance of xlab/ylab, numbers and line from plot margin,
#'            as in \code{\link{par}}, but with different defaults. DEFAULT: c(1.8, 0.8, 0)
#' @param bg Background. DEFAULT: par("bg")
#' @param border Border around inset plot. DEFAULT: par("fg")
#' @param las LabelAxisStyle. DEFAULT: 1
#' @param resetfocus Reset focus to original plot? Specifies where further
#'                   low level plot commands are directed to. DEFAULT: TRUE
#' @param colwise Logical: Continue next plot below current plot?
#'        If you had \code{par(mfcol=...)}, you must use \code{colwise=TRUE},
#'        otherwise the next plot will be to the right of the current plot
#'        (as with \code{par(mfrow=...)}). DEFAULT: FALSE
#' @param \dots further arguments passed to \code{\link{par}}.
#'        This may mess things up - please tell me for which arguments!
#'        You can do \code{par(las=1, las=2)} (the last will be set), so
#'        \code{smallPlot(plot(1), new=FALSE)} works, but may not yield the intended result.
#' 
smallPlot <- function(
expr,
x1=0.05, x2=0.70,
y1=0.50, y2=1.00,
outer=FALSE,
xpd=NA,
mar=c(3,3,1,1),
mgp=c(1.8, 0.8, 0),
bg=par("bg"),
border=par("fg"),
las=1,
resetfocus=TRUE,
colwise=FALSE,
...)
{
#     ------------
#  y2 |          |
#     |          |
#  y1 |          |
#     ------------
#       x1    x2
# vectors are ignored
x1 <- x1[1]
y1 <- y1[1]
x2 <- x2[1]
y2 <- y2[1]
# correct order of values:
if(x2<x1) {temp <- x2;  x2 <- x1;  x1 <- temp;  rm(temp)}
if(y2<y1) {temp <- y2;  y2 <- y1;  y1 <- temp;  rm(temp)}
# catch coordinates outside of the plot:
if(x1<0){warning("x1 (",x1,") set to 0."); x1 <- 0 }
if(y1<0){warning("y1 (",y1,") set to 0."); y1 <- 0 }
if(x2>1){warning("x2 (",x2,") set to 1."); x2 <- 1 }
if(y2>1){warning("y2 (",y2,") set to 1."); y2 <- 1 }
if(x1>1) stop("x1 (",x1,") cannot be >1.")
if(y1>1) stop("y1 (",y1,") cannot be >1.")
if(x2<0) stop("x2 (",x2,") cannot be <0.")
if(y2<0) stop("y2 (",y2,") cannot be <0.")
# catch too small differences
if(round(x2-x1,3)<0.001) stop("Difference too small between x1 (",x1,") and x2 (",x2,").")
if(round(y2-y1,3)<0.001) stop("Difference too small between y1 (",y1,") and y2 (",y2,").")
if(round(x2-x1,2)<0.05) warning("x1 (",x1,") and x2 (",x2,") are likely too close to each other.")
if(round(y2-y1,2)<0.05) warning("y1 (",y1,") and y2 (",y2,") are likely too close to each other.")
# margins recycled / truncated (if shorter or longer than 4):
mar <- mar_input <- rep(mar, length.out=4)
# smaller margins for outer margin plots
if(outer) if(!all(par("mfrow")==c(1,1))) mar <- mar/4 # not sure why 4, but it works well
# margins in relative units:
mar <- mar * rep(c(strheight("m",units="figure")*1.7,strwidth("m",units="figure")*1.5), 2)
# old parameters to be restored at exit:
op <- par(no.readonly=TRUE)
# par reset
if(resetfocus) on.exit(
  {
  par(op[names(op) != "mfg"]) # mfg not resetted, see https://stackoverflow.com/a/42798556/1587132
  if(colwise) par(mfcol=op$mfcol)
  if(!all(op$mfg==1)) par(mfg=op$mfg) # needed for multiple figure plots
  #par(mar=op$mar)
  par(new=op$new)
  text(0,0,"") # fixes bug of margin text not being written after mfg has been resetted
  })
# inset plot: background, border
if(!outer) par(plt=c(x1, x2, y1, y2), new=TRUE, mgp=mgp) else     # plt / fig
           par(fig=c(x1, x2, y1, y2), new=TRUE, mgp=mgp,
               omd=c(0,1,0,1), mar=c(0,0,0,0))
plot.new() # code line from ade4::add.scatter
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col=bg, border=border)
# inset plot: margins
marpos <- c(x1+mar[2], x2-mar[4], y1+mar[1], y2-mar[3])
tlh <- marpos[1]>=marpos[2] # margins tlh: too large horizontally
tlv <- marpos[3]>=marpos[4]
if(tlh|tlv) tstop(if(tlh)"horizontal", if(tlh&tlv)" and ", if(tlv)"vertical", 
     " margins are too large.\nTry a bigger graphic window or use margins smaller than mar=c(",
                 paste(mar_input, collapse=","),"). Cannot set par(plt/fig) to: ",
                 toString(round(marpos[1:4],3)),".", call.=FALSE)
# actually set margins:
if(!outer) par(plt=marpos, new=TRUE, las=las, xpd=xpd, ...) else
           par(fig=marpos, new=TRUE, las=las, xpd=xpd,
               omd=c(0,1,0,1), mar=c(0,0,0,0),  ...)
# Actual plot:
expr
# par of small plot:
sp <- par(no.readonly=TRUE)
# par reset through on.exit
return(invisible(sp))
}

