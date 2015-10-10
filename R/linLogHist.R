# R: Histogram transition from linear to logarithmic axis (animation)
# Berry Boessenkool, 2015-04-25, berry-b@gmx.de

linLogHist <- function(
x, # x values to be plotted in animation
steps=100, # Number of steps in transition
breaks=20, # \code{\link{hist}} breaks
col="blue", # \code{\link{hist}} color
las=1, # \code{\link{par}} LabelAxisStyle (numbers upright)
xlab=deparse(substitute(x)), # Label for the x axis
xlim=range(x, finite=TRUE), # xlim range in non-log units.
box=TRUE, # Draw box at the end to overplot \code{\link{abline}s} crossing the box?
parexpr, # Characterized Expression to set \code{\link{par}}, eg. \code{parexpr='par(mar=c(2,0.5,1.5,0.5), mpg=c(1.8,1,0))'}
endexpr, # Characterized Expression executed at the end of the plot, eg. \code{endexpr='mtext("Probability Density", line=-1, adj=0.03, outer=T)'}
sleep=0, # Pause time between frames, in seconds, passed to \code{\link{Sys.sleep}} 
axisargs=NULL, # List of arguments passed to \code{\link{logVals}}, like base
axisargs2=NULL, # List of arguments passed to \code{\link{logAxis}} in the final plot
firstplot=TRUE, # plot on linear scale first?
lastplot=TRUE, # plot on logarithmic scale at the end?
write_t=TRUE, # write transformation value in lower right corner?
values_t=NULL, # Supply vector with values for transformation (1/t). Overides steps. If you have a better algorithm than I do, please let me know!
...) # further arguments passed to \code{\link{hist}}, like freq, main, xlim, ylab. Excluded: x, xaxt, add
{
# x must be deparsed before it's evaluated (or something like that)
xlab <- xlab
# Tansformation values ---------------------------------------------------------
allt <- if(is.null(values_t))  linLogTrans(x, steps=steps, plot=FALSE)  else  values_t
# Plot the histograms ----------------------------------------------------------
# Plot on linear scale first:
if(firstplot)
  {
  if(!missing(parexpr)) eval(parse(text=parexpr))
  hist(x, breaks=breaks, col=col, las=las, xlab=xlab, xlim=xlim, ...)
  if(box) graphics::box("plot")
  if(!missing(endexpr)) eval(parse(text=endexpr))
  }
#
# Log labels and lines:
lv <- do.call(logVals, owa(list(from=x), axisargs))
# Images:
for(t in allt)
  {
  # Plot single frame:
  if(!missing(parexpr)) eval(parse(text=parexpr))
  hist(x^(1/t), breaks=breaks, col=col, las=las, xlab=xlab, xaxt="n", xlim=xlim^(1/t), ...)
  # draw grey lines at 10^n values and label appropriate ones:
  abline(v=(lv$all)^(1/t), col=8)
  axis(1, (lv$vals)^(1/t), lv$labs, las=las)
  hist(x^(1/t), breaks=breaks, col=col, add=TRUE, ...)
  if(box) graphics::box("plot")
  # write transformation value:
  if(write_t) title(sub=paste("t =", sprintf("%6.2f", t)), adj=1)
  if(!missing(endexpr)) eval(parse(text=endexpr))
  # slow down frame passing:
  if(sleep!=0) Sys.sleep(sleep)
  } # End for loop
# Final image
if(lastplot)
  {
  if(!missing(parexpr)) eval(parse(text=parexpr))
  hist(log10(x), breaks=breaks, col=col, las=las, xlab=xlab, xaxt="n", xlim=log10(xlim), ...)
  do.call(logAxis, args=owa(c(side=1, box=box), axisargs2))
  hist(log10(x), breaks=breaks, col=col, add=TRUE, ...)
  if(!missing(endexpr)) eval(parse(text=endexpr))
  }
return(invisible(allt))
} # end of function ------------------------------------------------------------
