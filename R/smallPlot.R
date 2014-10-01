# Berry Boessenkool, Aug 2014
# see also \code{\link[teachingDemos]{subplot}}  and   \code{\link[ade4]{add.scatter}}
# inset plot with margins, background and border
# mai does not work

smallPlot <- function(
expr, # expression creating a plot. Can be code within {braces}.
x1=5, # Position of topleft corner of small plot, relative to current figure region (0:100). Only used if add=TRUE
y1=100,
x2=70, # Bottom right corner. Ditto
y2=50,
mar=c(12, 14, 3, 3), # Margin vector in relative units (0:100), thus behaves differently than \code{\link{par}(mar)}
mgp=c(1.8, 0.8, 0), # MarGinPlacement: distance of xlab/ylab, numbers and line from plot margin, as in \code{\link{par}}, but with different defaults
bg=par("bg"), # Background
border=par("fg"), # Border around inset plot
las=1, # LabelAxisStyle
resetfocus=TRUE, # reset focus to original plot? Specifies where further low level plot commands are directed to.
...) # further arguments passed to \code{\link{par}. new=F} removes old plot. May mess things up - please tell me for which arguments!
{
# Input check:
if(y2>y1) {yy <- y2; y2 <- y1;  y1 <- yy; rm(yy);
    warning("Bottomright coordinate is higher (y) than topleft. y1 and y2 were swapped.")}
if(x2<x1) {xx <- x2; x2 <- x1;  x1 <- xx; rm(xx);
    warning("Bottomright coordinate is lower (x) than topleft. x1 and x2 were swapped.")}
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
if(resetfocus) par(op)
return(invisible(sp))
}

