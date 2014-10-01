linReg <- function(
   x,
   y,
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
   ...)
{
# make new plot if add is FALSE (the default):
if (!add) plot(x, y, las=1, pch=pch, xlab=xlab, ylab=ylab, ...)
# do linear regression and plotting
mod <- lm( y ~ x )
abline(mod, col=col, lwd=lwd, ...)
# expand digits vector, if necessary
if(is.na(digits[2])) digits[2] <- digits[1]
if(is.na(digits[3])) digits[3] <- digits[1]
if(is.na(digits[4])) digits[4] <- digits[1]
# Prepare formula writing
a <- round( coef(mod)[2] , digits[1])
b <- round( coef(mod)[1] , digits[2])
r <- round( summary(mod)$r.squared , digits[3])
Txt <- paste0("y = ", a, " * x ", ifelse(b>0," + ", " - "), abs(b), "\nR^2 = ",
          r, "\nRMSE = ", signif(rmse(x,y), digits[4]))
# write formula
legend(pos1, pos2, Txt, bty="n", text.col=col, inset=inset, xpd=xpd)
} # end of function
