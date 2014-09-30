# Plot density of beta distribution based on just alpha and beta
# Berry Boessenkool, July 28, 2014
betaPlot <- function(
  shape1=1.5, # alpha value as in \code{\link{dbeta}}
  shape2=5, # beta value
  lines=NA, # at which quantiles should vertical lines be plotted?
  fill=rgb(0,0.3,0.8, 0.4), # color passed to \code{\link{polygon}}
  cumulative=TRUE, # should cumulative density distribution be added?
  mar=c(2,3,3,3), # margins for plot passed to \code{\link{par}}
  las=1, # arguments passed to \code{\link{plot}}
  main=paste("Beta density with\nalpha =", shape1, "and beta =", shape2), # main as in \code{\link{plot}}.
  ylim=lim0(y), # limit for the y axis
  xlim=0:1,
  ylab="", # labels for the axes
  xlab="",
  type="l", # arguments passed to \code{\link{plot}} and \code{\link{lines}} 
  lty=1,
  col=par("fg"),
  ... # further arguments passed to \code{\link{plot}} like lwd, cex.axis, etc.
  )
{
par(mar=mar)
# create x and y coordinates
x <- seq(1e-3,1-1e-3, length=200)
y <- dbeta(x, shape1, shape2)
# plot
plot(x, y, las=las, type=type, col=col, lty=lty, xaxs="i", 
     ylab=ylab, xlab=xlab, main=main, ylim=ylim, xlim=xlim, ...)
# vertical lines
abline(v=qbeta(lines, shape1, shape2), col=8)
# Polygon
#browser()
polygon(c(0,x,1), c(0,y, 0), col=fill, border=NA)
lines(x, y, type=type, col=col, lty=lty)
if(cumulative)
  {
  lines(x, pbeta(x, shape1, shape2)*par("usr")[4], type="l", col=2)
  axis(4, at=0:4/4*par("usr")[4], labels=0:4/4, col.axis=2, las=1, col=2)
  }
box()
}
