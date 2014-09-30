# Plot density of normal distribution based on just mean and sd
# Berry Boessenkool, July 23, 2014
normPlot <- function(
  mean=0, # average value as in \code{\link{dnorm}}
  sd=1, # standard deviation
  width=3, # distance (in sd) from plot ends to mean
  lines=TRUE, # Should vertical lines be plotted at mean +- n*sd?
  fill=rgb(0,0.3,0.8, 0.4), # color passed to \code{\link{polygon}}
  cumulative=TRUE, # should cumulative density distribution be added?
  las=1, # arguments passed to \code{\link{plot}}
  main=paste("Normal density with\nmean =", mean, "and sd =", sd), # main as in \code{\link{plot}}.
  ylim=lim0(y), # limit for the y axis
  ylab="", # labels for the axes
  xlab="",
  type="l", # arguments passed to \code{\link{plot}} and \code{\link{lines}} 
  lty=1,
  col=par("fg"),
  ... # further arguments passed to \code{\link{plot}} like lwd, xaxs, cex.axis, etc.
  )
{
par(mar=c(2,3,3,3))
# create x and y coordinates
x <- seq(mean-width*sd, mean+width*sd, length=200)
y <- dnorm(x, mean, sd)
# plot
plot(x, y, las=las, type=type, col=col, lty=lty, 
     ylab=ylab, xlab=xlab, main=main, ylim=ylim, ...)
# vertical lines
if(lines) abline(v=mean+(-width:width)*sd, col=8)
# recalculate coordinates with extrema
x <- c(mean-20*sd, x, mean+20*sd)
y <- dnorm(x, mean, sd)
# and plot that too
polygon(c(x,x[1]), c(y, y[1]), col=fill, border=NA)
lines(x, y, type=type, col=col, lty=lty)
if(cumulative)
  {
  lines(x, pnorm(x, mean, sd)*par("usr")[4], type="l", col=2)
  axis(4, at=0:4/4*par("usr")[4], labels=0:4/4, col.axis=2, las=1, col=2)
  }
box()
}

