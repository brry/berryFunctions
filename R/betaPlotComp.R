# Compare beta distributions
# Berry Boessenkool, Dec 2015
betaPlotComp <- function(
  shape1=c(0.5, 1:4, 10,20), # Vector of alpha values as in \code{\link{dbeta}}
  shape2=shape1, # Beta values to be compared
  cumulative=FALSE, # Should the cumulative density distribution line be added?
  cex=0.8, # Character EXpansion size
  las=1, # Label Axis Style passed to \code{\link{axis}}
  main="", # Main as in \code{\link{plot}}.
  ylim=lim0(4), # LIMit for the Y axis
  mar=rep(0,4), # MARgins for plot passed to \code{\link{par}}
  oma=c(2,2,4.5,2), # Outer MArgins for plot passed to \code{\link{par}} 
  mgp=c(3,0.7,0), # MarGin Placement
  keeppar=FALSE, # Should margin parameters be kept instead of being restored to previous value?
  textargs=NULL, # List of arguments passed to \code{\link{textField}} 
  ... # Further arguments passed to \code{\link{betaPlot}} like lines, fill, etc.
  )
{
a <- shape1
b <- shape2
op <- par(mfrow=c(length(b), length(a)), yaxt="n", xaxt="n", cex=cex, mar=mar, oma=oma, mgp=mgp)
on.exit(if(!keeppar) par(op))
for(y in b)
  for(x in a)
  {
  betaPlot(x,y, main=main, ylim=ylim, mar=mar, cumulative=cumulative, ...)
  #mtext(text=paste("a=",x,", b=",y, sep=""), side=3, line=-1.5, cex=0.7)
  textdef <- list(x=0.5, y=0.9*ylim[2], labels=paste("a=",x,", b=",y, sep=""), 
                  mar=0.1, fill=addAlpha("white", 0.7), cex=1)
  do.call(textField, owa(textdef, textargs))
  if(y==min(b)) mtext(paste("a =",x), side=3, line=0.5, cex=cex)
  if(x==min(a)) mtext(paste("b =",y), side=2, line=0.5, cex=cex)
  if(y==max(b) & x==quantile(a, 0.5, type=1)) axis(1, at=c(0,0.5,1), xaxt="s", las=las)
  if(x==max(a) & y==quantile(b, 0.5, type=1)) axis(4, yaxt="s", las=las)
  }
mtext("beta density distribution", line=2.5, outer=TRUE, cex=1.2, font=2)
#box("outer")
}
