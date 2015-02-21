# Berry Boessenkool, Aug 2014
# see also \code{\link[teachingDemos]{subplot}}  and   \code{\link[ade4]{add.scatter}}
# inset plot with margins, background and border
# mai does not work

smallPlot <- function(
expr, # expression creating a plot. Can be code within {braces}.
x=c(5,70), # Position of small plot, relative to current figure region (0:100)
y=c(50,100),
x1,y1,x2,y2, # Positions of topleft and bottomright corner. Replaced with x,y, kept here for backcompatibility.
mar=c(12, 14, 3, 3), # Margin vector in relative units (0:100), thus behaves differently than \code{\link{par}(mar)}
mgp=c(1.8, 0.8, 0), # MarGinPlacement: distance of xlab/ylab, numbers and line from plot margin, as in \code{\link{par}}, but with different defaults
bg=par("bg"), # Background
border=par("fg"), # Border around inset plot
las=1, # LabelAxisStyle
resetfocus=TRUE, # reset focus to original plot? Specifies where further low level plot commands are directed to.
...) # further arguments passed to \code{\link{par}. new=F} removes old plot. May mess things up - please tell me for which arguments!
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
# control for 0:1 input:
if(diff(range(x, na.rm=TRUE)) < 1  |  diff(range(y, na.rm=TRUE)) < 1  )
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
  if( par("mfrow")[1]==1 & par("mfrow")[2]==1  ) par(op) # ruins multiple figure plots, so:
  else par(plt=op$plt, new=op$new, mgp=op$mgp, las=op$las)
  }
return(invisible(sp))
}

