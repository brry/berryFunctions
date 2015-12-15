linReg <- function(
x,                # Numeric or formula (see examples). Vector with values of explanatory variable
y=NULL,           # Numeric. Vector with values of dependent variable.
data=NULL,        # Dataframe. If x is a formula, the according columns from data are used as x and y
add=FALSE,        # Logical. If TRUE, line and text are added to the existing graphic. DEFAULT: FALSE (plots datapoints first and then the line.)
digits=2,         # Numeric vector of length \eqn{\ge 1}. Specifies number of digits a,b,r,e are rounded to in the formula "y=a*x+b \n R^2=r \n RMSE=e", respectively. If values are not specified, they are set equal to the first.
pch=16,                       # Point Character of datapoints, see \code{\link{par}}
col=2,                        # Color of the regression line, see \code{\link{par}}
colband=addAlpha(col),        # Color of the confidence region band. DEFAULT: addAlpha(col)
level=0.95,                   # Confidence level, see \code{\link{predict.lm}}
lwd=1,                        # Numeric. Linewidth, see \code{\link{par}}
xlab=deparse(substitute(x)),  # Axis label if add=FALSE
ylab=deparse(substitute(y)),  # Axis label if add=FALSE
main="linear regression",     # Title if add=FALSE. Changed (if not specified) for x=formula with data.
pos1="top",                   # \code{\link{xy.coords}}-acceptable position of the formula
pos2=NULL,                    # For numerical coordinates, this is the y-position. DEFAULT: NULL, as in \code{\link{legend}}
inset=0,                      # Numeric vector of length \eqn{\le 2}. inset distance(s) from the margins as a fraction of the plot region when formula legend is placed by keyword.
legargs=NULL,                 # list of arguments passed to legend, like list(cex=0.8, xpd=TRUE, bg="white"), ...   xpd specifies whether formula can be written only inside the plot region (when FALSE) or inside the figure region including mar (when TRUE) or in the entire device region including oma (when NA)
...)                          # Further arguments passed to \code{\link{plot}} and \code{\link{abline}}.
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
pred <- predict(mod, newdata=data.frame(x=x2), interval="confidence", level=level )
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
legdef <- list(x=pos1, y=pos2, legend=Txt, bty="n", text.col=col, inset=inset)
do.call(legend, owa(legdef, legargs, "legend", "inset"))
} # end of function
