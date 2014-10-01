# R: Transition from linear to logarithmic axis (animation)
# Berry Boessenkool, 19.6.2014, berry-b@gmx.de

linLogTrans <- function(
   x, # x values to be plotted in animation
   y, # vector with corresponding y values
   log="x", # which axis is logarithmic, "x" or "y"
   steps=100, # number of steps in transition
   base=1, # base passed to \code{\link{logVals}}
   firstplot=TRUE, # plot on linear scale first?
   lastplot=TRUE, # plot on logarithmic scale at the end?
   write_t=TRUE, # write transformation value in lower right corner?
   values_t=NULL, # Supply vector with values for transformation (1/t). Overides steps. If you have a better algorithm than I do, please let me know!
   pointsarg=NULL, # List of further arguments passed to points, like pch, cex, col
   ...) # further arguments passed only to plot, like main, xlim, ylab. Excluded: x, y, las, xaxt, type
{
# Tansformation values ---------------------------------------------------------
if(is.null(values_t)) # if it's not given by user, use internal calculation:
  {
  if(length(steps)>1) steps <- max(steps, na.rm=TRUE)
  # t-values for x^(1/t):
  allt <- 10^(seq(0,2.5,len=1e4) )
  # selection at upper half of these values;
  # Otherwise, the animation slows down too much at the end
  f <- 1.4 # multiplication factor due to length loss by unique
  sel <- round(seq(1, 10, len=f*steps)^4)   #0.5*seq(1, 100, len=1.3*steps)^2 + 0.5*
  sel2 <- unique(round(log10(seq(1, 10, len=f*steps))*f*steps))
  sel2[1] <- 1
  sel <- sel[sel2]
  # final t-values for transition:
  allt <- unique(round(allt[sel], 2))
  }
  else allt <- values_t
# Plot the images --------------------------------------------------------------
# Plot on linear scale first:
if(firstplot)
  {
  plot(x, y, las=1, type="n", ...)
  do.call(points, args=owa(d=list(x=x, y=y), a=pointsarg, u=c("x", "y")))
  }
# in case people capitalize log:
log <- tolower(log)
if(log=="x") # -----------------------------------------------------------------
  {
  # Log labels and lines:
  lv <- logVals(x, base=base)
  # Images:
  for(t in allt)
     {
     # Plot single frame:
     plot(x^(1/t), y, las=1, xaxt="n", type="n", ...)
     # draw grey lines at 10^n values and label appropriate ones:
     abline(v=(lv$all)^(1/t), col=8)
     box()
     axis(1, (lv$vals)^(1/t), lv$labs)
     # user-specified arguments for points:
     pargs <- owa(d=list(x=x^(1/t), y=y), a=pointsarg, u=c("x", "y"))
     # draw original points:
     do.call(points, args=pargs)
     # write transformation value:
     if(write_t) title(sub=paste("t =", sprintf("%6.2f", t)), adj=1)
     } # End for loop
  # Final image
  if(lastplot)
    {
    plot(x, y, las=1, xaxt="n", type="n", log="x", ...)
    abline(v=lv$all, col=8) ; box() ; axis(1, lv$vals, lv$labs)
    do.call(points, args=owa(d=list(x=x, y=y), a=pointsarg, u=c("x", "y")))
    }
  }
else if(log=="y") # ------------------------------------------------------------
  {
  lv <- logVals(y, base=base)
  for(t in allt)
     {
     plot(x, y^(1/t), las=1, yaxt="n", type="n", ...)
     abline(h=(lv$all)^(1/t), col=8) ; box() ; axis(2, (lv$vals)^(1/t), lv$labs)
     pargs <- owa(d=list(x=x, y=y^(1/t)), a=pointsarg, u=c("x", "y"))
     do.call(points, args=pargs)
     if(write_t) title(sub=paste("t =", sprintf("%6.2f", t)), adj=1)
     } # End for loop
  if(lastplot)
    {
    plot(x, y, las=1, yaxt="n", type="n", log="y", ...)
    abline(h=lv$all, col=8) ; box() ; axis(2, lv$vals, lv$labs)
    do.call(points, args=owa(d=list(x=x, y=y), a=pointsarg, u=c("x", "y")))
    }
  }
else if(log=="xy" | log=="yx") # -----------------------------------------------
  {
  lvx <- logVals(x, base=base)
  lvy <- logVals(y, base=base)
  for(t in allt)
     {
     plot(x^(1/t), y^(1/t), las=1, xaxt="n", yaxt="n", type="n", ...)
     abline(h=(lvy$all)^(1/t), v=(lvx$all)^(1/t), col=8) ; box()
     axis(1, (lvx$vals)^(1/t), lvx$labs) ; axis(2, (lvy$vals)^(1/t), lvy$labs)
     pargs <- owa(d=list(x=x^(1/t), y=y^(1/t)), a=pointsarg, u=c("x", "y"))
     do.call(points, args=pargs)
     if(write_t) title(sub=paste("t =", sprintf("%6.2f", t)), adj=1)
     } # End for loop
  if(lastplot)
    {
    plot(x, y, las=1, xaxt="n", yaxt="n", type="n", log="xy", ...)
    abline(h=lvy$all, v=lvx$all, col=8) ; box()
    axis(1, lvx$vals, lvx$labs) ; axis(2, lvy$vals, lvy$labs)
    do.call(points, args=owa(d=list(x=x, y=y), a=pointsarg, u=c("x", "y")))
    }
  }
else stop("log can only be 'x', 'y', or 'xy'.")
return(invisible(allt))
} # end of function ------------------------------------------------------------


# old way to get the transformation values:
# make more steps, as ca 35% are removed from allt later:
# steps <- round(steps * 1.35)#(1+(0.5+0.2*0.5)))
# steps <- 150
# allt <- 10^(seq(0,2.5,len=steps) )
# sel <- round(10^(seq(log10(steps/2), log10(steps), len=0.2*steps) ))
# allt <- allt[c(1:(sel[1]-1), sel)]
