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
   colband=addAlpha(col), # Color of the confidence region band. DEFAULT: addAlpha(col)
   lwd=1,
   xlab=deparse(substitute(x)),
   ylab=deparse(substitute(y)),
   main="linear regression",
   ...)
{
if(class(x)=="formula")
  {
  mf <- model.frame(x, data=data)
  x <- mf[,2]
  y <- mf[,1]
  if(missing(xlab)) xlab <- colnames(mf)[2]
  if(missing(ylab)) ylab <- colnames(mf)[1]
  if(!missing(data) & missing(main)) main <- paste("linear regression of",
                                                    deparse(substitute(data)))
  }
# make new plot if add is FALSE (the default):
if (!add) plot(x, y, las=1, pch=pch, xlab=xlab, ylab=ylab, main=main, ...)
# do linear regression and plotting
mod <- lm( y ~ x )
abline(mod, col=col, lwd=lwd, ...)
x2 <- seqR(par("usr")[1:2], len=100)
pred <- predict(mod, newdata=data.frame(x=x2), interval="confidence" )
ciBand(yu=pred[,3], yl=pred[,2], x=x2, colb=colband, add=TRUE)
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
