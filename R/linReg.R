linReg <- function(
   x,
   y=NULL,
   data=NULL,
   digits=2,
   inset=0,
   xpd=par("xpd"),
   pos1="top",
   pos2=NULL,
   add=FALSE,
   pch=16,
   col=2,
   lwd=1,
   xlab=deparse(substitute(x)),
   ylab=deparse(substitute(y)),
   main="linear regression",
   ...)
{
if(class(x)=="formula")
  {
  if(!missing(data))
    {                   # get x and y from data.frame
    name <- as.character(x)[-1]
    x <- data[ , name[2] ]  ;  if(missing(xlab)) xlab <- name[2]
    y <- data[ , name[1] ]  ;  if(missing(ylab)) ylab <- name[1]
    if(missing(main)) main <- paste("linear regression of",
                                    deparse(substitute(data)))
    } else
    {                   # get x and y from formula directly
    name <- as.character(x)[-1]
    x <- get(name[2]) ;  if(missing(xlab)) xlab <- name[2]
    y <- get(name[1]) ;  if(missing(ylab)) ylab <- name[1]
    }
  }
# make new plot if add is FALSE (the default):
if (!add) plot(x, y, las=1, pch=pch, xlab=xlab, ylab=ylab, main=main, ...)
# do linear regression and plotting
mod <- lm( y ~ x )
abline(mod, col=col, lwd=lwd, ...)
# expand digits vector, if necessary
digits <- rep(digits, length.out=4)
# Prepare formula writing
a <- round( coef(mod)[2] , digits[1])
b <- round( coef(mod)[1] , digits[2])
r <- round( summary(mod)$r.squared , digits[3])
Txt <- paste0("y = ", a, " * x ", ifelse(b>0," + ", " - "), abs(b), "\nR\U00B2 = ",
          r, "\nRMSE = ", signif(rmse(x,y), digits[4]))
# write formula
legend(pos1, pos2, Txt, bty="n", text.col=col, inset=inset, xpd=xpd)
} # end of function
