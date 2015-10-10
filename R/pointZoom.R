# Zoom into scatterplot
# Berry Boessenkool
pointZoom <- function(
          x,
          y=NA,
          z=NA,
          Time=1,
          steps=30,
          las=1,
          usecolp=FALSE,
          xlab=substitute(x),
          ylab=substitute(y),
          quiet=FALSE,
          expr,
          ...)
{
if(interactive()){ # to silence the R CMD check warnings
if(!quiet){
  legend("top", "Instructions appear in console", bty="n", text.col="orange")
  message("Please select the area to zoom to in the graphics window.")
  message("first klick topleft, then bottomright."); flush.console()   
  } # if notify end
w <- locator(2)
u <- par()$usr
if(w$x[1] > w$x[2] | w$y[1] < w$y[2])
  {
  message("wrong selection!")
  message("first klick topleft, then bottomright of the area to zoom to.")
  flush.console(); w <- locator(2)
  }
# if x is matrix:
if(class(x)[1] %in% c("matrix", "data.frame", "array") )
   {y <- x[,2]; if(ncol(x)>2) z <- x[,3]; x <- x[,1]}
#
if (usecolp)
  {polygon(c(w$x, rev(w$x)), rep(w$y, each = 2))
  Sys.sleep(1)
  colPoints(x, y, z, add=FALSE, xlim=w$x, ylim=rev(w$y), las=las, ylab=ylab, xlab=xlab, ...)
  } else  {
X1 <- c(u[1]+(w$x[1]-u[1])*1:steps/steps)
X2 <- c(u[2]-(u[2]-w$x[2])*1:steps/steps)
Y1 <- c(u[3]+(w$y[2]-u[3])*1:steps/steps)
Y2 <- c(u[4]-(u[4]-w$y[1])*1:steps/steps)
for ( i in 1:steps) 
   {
   rect(xleft=w$x[1], ybottom=w$y[1], xright=w$x[2], ytop=w$y[2])
   Sys.sleep(Time/steps)
   plot(x, y, xlim=c(X1[i], X2[i]), ylim=c(Y1[i], Y2[i]), las=las,
   ylab=ylab, xlab=xlab ,  yaxs="i", xaxs="i", ...)
   rect(xleft=w$x[1], ybottom=w$y[1], xright=w$x[2], ytop=w$y[2])
   if(!missing(expr)) eval(parse(text=expr))
   } # loop end
}
# if(!quiet) message("Tell me if this was helpful: berry-b@gmx.de") 
} # end if interactive
} # function end
