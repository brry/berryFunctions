# R: Transition from linear to logarithmic axis (animation)
# Berry Boessenkool, 19.6.2014, berry-b@gmx.de

linLogTrans <- function(
   x, # x values to be plotted in animation
   y, # vector with corresponding y values
   log="x", # which axis is logarithmic, "x" or "y"
   steps=100, # number of steps in transition
   base=1, # base passed to \code{\link{logVals}}
   las=1, # \code{\link{par}} LabelAxisStyle (numbers upright)
   plot=TRUE, # Plot animations at all? False to just get the t-vector (used in \code{\link{linLogHist}})
   xlim=range(x, finite=TRUE), # xlim range in non-log units.
   ylim=range(y, finite=TRUE), # ylim range in non-log units.
   box=TRUE, # Draw box at the end to overplot \code{\link{abline}s} crossing the box?
   parexpr, # Characterized Expression to set \code{\link{par}}, eg. \code{parexpr='par(mar=c(2,0.5,1.5,0.5), mpg=c(1.8,1,0))'}
   endexpr, # Characterized Expression executed at the end of the plot, eg. \code{endexpr='mtext("Probability Density", line=-1, adj=0.03, outer=T)'}
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
  if(steps>1000) steps <- 1000
  # t-values for x^(1/t):
  # coefficients of degree 17 polynomial, see examples
  tvc <- c(0.995726006684206, 0.00310820777426466, -2.7073341927363e-05,
  6.11849831959088e-07, -7.05912829318337e-09, 4.82269115381641e-11,
  -2.02029859969675e-13, 5.30790764027315e-16, -8.53304767303152e-19,
  7.29652239791065e-22, -8.04764444262045e-26, -4.35519950517021e-28,
  3.26048883565918e-31, NA, -6.70898382748396e-38, NA, 1.04885136585542e-44,NA)
  tvx <- 1:1000 # t x values
  tvy <- rowSums(sapply(1:18, function(i) tvc[i]*tvx^(i-1)), na.rm=TRUE)
  tvy[1] <- 1
  # final t-values for transition:
  allt <- tvy[seq(1,1000, length=steps)]
  }
  else allt <- values_t
# return t values only:
if(!plot) return(allt)
# Plot the images --------------------------------------------------------------
# Plot on linear scale first:
if(firstplot)
  {
  if(!missing(parexpr)) eval(parse(text=parexpr))
  plot(x, y, las=las, type="n", xlim=xlim, ylim=ylim, ...)
  do.call(points, args=owa(d=list(x=x, y=y), a=pointsarg, "x", "y"))
  if(box) graphics::box("plot")
  if(!missing(endexpr)) eval(parse(text=endexpr))
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
     if(!missing(parexpr)) eval(parse(text=parexpr))
     # Plot single frame:
     plot(x^(1/t), y, las=las, xaxt="n", type="n", xlim=xlim^(1/t), ylim=ylim, ...)
     # draw grey lines at 10^n values and label appropriate ones:
     abline(v=(lv$all)^(1/t), col=8)
     axis(1, (lv$vals)^(1/t), lv$labs, las=las)
     # user-specified arguments for points:
     pargs <- owa(d=list(x=x^(1/t), y=y), a=pointsarg, "x", "y")
     # draw original points:
     do.call(points, args=pargs)
     if(box) graphics::box("plot")
     if(!missing(endexpr)) eval(parse(text=endexpr))
     # write transformation value:
     if(write_t) title(sub=paste("t =", sprintf("%6.2f", t)), adj=1)
     } # End for loop
  # Final image
  if(lastplot)
    {
    if(!missing(parexpr)) eval(parse(text=parexpr))
    plot(x, y, las=las, xaxt="n", type="n", log="x", xlim=xlim, ylim=ylim, ...)
    abline(v=lv$all, col=8)
    axis(1, lv$vals, lv$labs, las=las)
    do.call(points, args=owa(d=list(x=x, y=y), a=pointsarg, "x", "y"))
    if(box) graphics::box("plot")
    if(!missing(endexpr)) eval(parse(text=endexpr))
    }
  }
else if(log=="y") # ------------------------------------------------------------
  {
  lv <- logVals(y, base=base)
  for(t in allt)
     {
     if(!missing(parexpr)) eval(parse(text=parexpr))
     plot(x, y^(1/t), las=las, yaxt="n", type="n", xlim=xlim, ylim=ylim^(1/t), ...)
     abline(h=(lv$all)^(1/t), col=8)
     axis(2, (lv$vals)^(1/t), lv$labs, las=las)
     pargs <- owa(d=list(x=x, y=y^(1/t)), a=pointsarg, "x", "y")
     do.call(points, args=pargs)
     if(box) graphics::box("plot")
     if(!missing(endexpr)) eval(parse(text=endexpr))
     if(write_t) title(sub=paste("t =", sprintf("%6.2f", t)), adj=1)
     } # End for loop
  if(lastplot)
    {
    if(!missing(parexpr)) eval(parse(text=parexpr))
    plot(x, y, las=las, yaxt="n", type="n", log="y", xlim=xlim, ylim=ylim, ...)
    abline(h=lv$all, col=8)
    axis(2, lv$vals, lv$labs, las=las)
    do.call(points, args=owa(d=list(x=x, y=y), a=pointsarg, "x", "y"))
    if(box) graphics::box("plot")
    if(!missing(endexpr)) eval(parse(text=endexpr))
    }
  }
else if(log=="xy" | log=="yx") # -----------------------------------------------
  {
  lvx <- logVals(x, base=base)
  lvy <- logVals(y, base=base)
  for(t in allt)
     {
     if(!missing(parexpr)) eval(parse(text=parexpr))
     plot(x^(1/t), y^(1/t), las=las, xaxt="n", yaxt="n", type="n", xlim=xlim^(1/t), ylim=ylim^(1/t), ...)
     abline(h=(lvy$all)^(1/t), v=(lvx$all)^(1/t), col=8)
     axis(1, (lvx$vals)^(1/t), lvx$labs, las=las)
     axis(2, (lvy$vals)^(1/t), lvy$labs, las=las)
     pargs <- owa(d=list(x=x^(1/t), y=y^(1/t)), a=pointsarg, "x", "y")
     do.call(points, args=pargs)
     if(box) graphics::box("plot")
     if(!missing(endexpr)) eval(parse(text=endexpr))
     if(write_t) title(sub=paste("t =", sprintf("%6.2f", t)), adj=1)
     } # End for loop
  if(lastplot)
    {
    if(!missing(parexpr)) eval(parse(text=parexpr))
    plot(x, y, las=las, xaxt="n", yaxt="n", type="n", log="xy", xlim=xlim, ylim=ylim, ...)
    abline(h=lvy$all, v=lvx$all, col=8)
    axis(1, lvx$vals, lvx$labs, las=las)
    axis(2, lvy$vals, lvy$labs, las=las)
    do.call(points, args=owa(d=list(x=x, y=y), a=pointsarg, "x", "y"))
    if(box) graphics::box("plot")
    if(!missing(endexpr)) eval(parse(text=endexpr))
    }
  }
else stop("log can only be 'x', 'y', or 'xy'.")
return(invisible(allt))
} # end of function ------------------------------------------------------------

# old way to get the transformation values:
#  if(length(steps)>1) steps <- max(steps, na.rm=TRUE)
#  # t-values for x^(1/t):
#  allt <- 10^(seq(0,2.5,len=1e4) )
#  # selection at upper half of these values;
#  # Otherwise, the animation slows down too much at the end
#  f <- 1.4 # multiplication factor due to length loss by unique
#  sel <- round(seq(1, 10, len=f*steps)^4)   #0.5*seq(1, 100, len=1.3*steps)^2 + 0.5*
#  sel2 <- unique(round(log10(seq(1, 10, len=f*steps))*f*steps))
#  sel2[1] <- 1
#  sel <- sel[sel2]
#  # final t-values for transition:
#  allt <- unique(round(allt[sel], 2))

# Current t value calculation is based on a polynomial of degree 17 fitted to
# the t values for 300 steps from this version

# very old way to get the transformation values:
# make more steps, as ca 35% are removed from allt later:
# steps <- round(steps * 1.35)#(1+(0.5+0.2*0.5)))
# steps <- 150
# allt <- 10^(seq(0,2.5,len=steps) )
# sel <- round(10^(seq(log10(steps/2), log10(steps), len=0.2*steps) ))
# allt <- allt[c(1:(sel[1]-1), sel)]

