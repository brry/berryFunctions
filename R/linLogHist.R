# R: Histogram transition from linear to logarithmic axis (animation)
# Berry Boessenkool, 2015-04-25, berry-b@gmx.de

linLogHist <- function(
x, # x values to be plotted in animation
steps=100, # number of steps in transition
breaks=20, # \code{\link{hist}} breaks
col="blue", # \code{\link{hist}} color
las=1, # \code{\link{par}} LabelAxisStyle (numbers upright)
xlab=deparse(substitute(x)), # Label for the x axis
box=TRUE,
axisargs=NULL, # List of arguments passed to \code{\link{logVals}} and \code{\link{logAxis}}, like base
firstplot=TRUE, # plot on linear scale first?
lastplot=TRUE, # plot on logarithmic scale at the end?
write_t=TRUE, # write transformation value in lower right corner?
values_t=NULL, # Supply vector with values for transformation (1/t). Overides steps. If you have a better algorithm than I do, please let me know!
...) # further arguments passed to \code{\link{hist}}, like freq, main, xlim, ylab. Excluded: x, xaxt, possibly add
{
# x must be deparsed before it's evaluated (or something like that)
xlab <- xlab
# Tansformation values ---------------------------------------------------------
allt <- if(is.null(values_t))  linLogTrans(x, steps=steps, plot=FALSE)  else  values_t
# Plot the histograms ----------------------------------------------------------
# Plot on linear scale first:
if(firstplot)
  {
  hist(x, breaks=breaks, col=col, las=las, xlab=xlab, ...)
  if(box) graphics::box("plot")
  }
#
# Log labels and lines:
lv <- do.call(logVals, owa(list(from=x), axisargs))
# Images:
for(t in allt)
  {
  # Plot single frame:
  hist(x^(1/t), breaks=breaks, col=col, las=las, xlab=xlab, xaxt="n", ...)
  # draw grey lines at 10^n values and label appropriate ones:
  abline(v=(lv$all)^(1/t), col=8)
  axis(1, (lv$vals)^(1/t), lv$labs, las=las)
  hist(x^(1/t), breaks=breaks, col=col, add=TRUE, ...)
  if(box) graphics::box("plot")
  # write transformation value:
  if(write_t) title(sub=paste("t =", sprintf("%6.2f", t)), adj=1)
  } # End for loop
# Final image
if(lastplot)
  {
  hist(log10(x), breaks=breaks, col=col, las=las, xlab=xlab, xaxt="n", ...)
  do.call(logAxis, args=owa(c(side=1, box=box), axisargs))
  hist(log10(x), breaks=breaks, col=col, add=TRUE, ...)
  }
return(invisible(allt))
} # end of function ------------------------------------------------------------
