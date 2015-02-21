# Berry Boessenkool, Aug 2014

colPointsHist <- function(
z, # Values of third dimension used in \code{\link{colPoints}}
nbins=40, # Number of classes (thus, colors)
colors=rainbow2(nbins), # Colors that are used for the background
bb=seqR(z, length.out=nbins+1), # Borders of bins for the background
at=pretty2(z), # Positions of x-axis labels
labels=at, # X-axis labels themselves

bg="white", # Background behind background and axis labels
x=0:40, y=0:30, # relative coordinates (0:100) of inset plot, see \code{\link{smallPlot}}
x1,y1,x2,y2, # Positions of topleft and bottomright corner. Replaced with x,y, kept here for backcompatibility.
mar=c(6, 7, 3, 2), # Margins for \code{\link{smallPlot}} in relative values (0:100)
mgp=c(1.8, 0.6, 0), # MarGinPlacement: distance of xlab/ylab, numbers and line from plot margin, as in \code{\link{par}}, but with different defaults
sborder=NA, # Border around inset subplot
resetfocus=TRUE, # Reset focus to original plot? Specifies where further low level plot commands are directed to.

breaks=20, # Breaks as in \code{\link{hist}}, but with a different default
freq=TRUE, # Plot count data in hist? (if FALSE, plot density instead)
col=par("fg"), # Color of histogram bars
border=NA, # Border around each bar
main="", ylab="", xlab="", # Labels
las=1, # LabelAxisStyle
axes=TRUE, # Draw axes?
...) # Further arguments passed to \code{\link{hist}}. NOT POSSIBLE: \code{x, add}
{
z <- as.numeric(z)
if(length(colors) != nbins) stop("Number of colors is not equal to number of classes.")
# plot setup:
smallPlot(x=x, y=y, x1=x1,y1=y1, x2=x2,y2=y2, mar=mar, mgp=mgp, bg=bg,
  border=sborder, las=las, resetfocus=resetfocus, expr={
  hist(z, breaks=breaks, main=main, xaxt="n", ylab=ylab, xlab=xlab,
             freq=freq, las=las, col=col, border=border, ...)
  if(axes) axis(side=1, at=at, labels=labels, lwd=0, lwd.ticks=1)
  # colored background:
  for(i in 1:length(colors) )
    rect(xleft=bb[i], ybottom=par("usr")[3],
        xright=bb[i+1],  ytop=par("usr")[4], col=colors[i], border=NA)
  # overdraw with black histogram:
  hist(z, breaks=breaks, freq=freq, add=TRUE, col=col, border=border, ...)
  })
}
