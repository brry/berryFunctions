# Exponential Regression
# Berry Boessenkool, Dec 2014

expReg <- function(
x, # Numeric or formula (see examples). Vector with values of explanatory variable
y=NULL, # Numeric. Vector with values of dependent variable
data=NULL, # Dataframe. If x is a formula, the according columns from data are used as x and y.
logy=TRUE, # Plot with a logarithmic y axis?  Calls \code{\link{logAxis}}
predictnew=NULL, # Vector with values to predict outcome for. Passed as \code{newdata} to \code{\link{predict.lm}}.
interval="confidence", # Inter val for prediction
plot=TRUE, # Plot things at all? If FALSE, predictnew will still be returned
digits=2, # Numeric vector of length \eqn{\ge 1}. Specifies number of digits a,b,r,e are rounded to in the formula "y=a*log(x)+b \\n R^2=r \\n RMSE=e", respectively. If values are not specified, they are set equal to the first
inset=0, # Numeric vector of length \eqn{\le 2}. inset distance(s) from the margins as a fraction of the plot region when formula is placed by keyword
xpd=par("xpd"), # Logical, specifying wheter formula can be written only inside the plot region (when FALSE) or inside the figure region including mar (when TRUE) or in the entire device region including oma (when NA)
pos1="top", # \code{\link{xy.coords}}-acceptable position of the formula
pos2=NULL, # For numerical coordinates, this is the y-position. DEFAULT: NULL, as in \code{\link{legend}}
add=FALSE, # Logical. If TRUE, line and text are added to the existing graphic. DEFAULT: FALSE (plots datapoints first and then the line.)
pch=16, # Point Character, see \code{\link{par}}
col=rgb(0,0,0, 0.5), # Color of points, see \code{\link{par}}
modcol=2, # color of model line
lwd=1, # Numeric. Linewidth, see \code{\link{par}}
xlab=deparse(substitute(x)), # Character / Expression. axis label and graph title if add=FAlSE}
ylab=deparse(substitute(y)),
main="exponential regression",
xlim=range(x), # graphic range
ylim=range(y),
...)# Further arguments passed to \code{\link{plot}} and \code{\link{abline}}.}
{
# deparse labs before x and y are evaluated:
xlab <- xlab ;  ylab <- ylab
# Formula:
if(class(x)=="formula")
  {
  if(!missing(data))
    {                   # get x and y from data.frame
    name <- as.character(x)[-1]
    x <- data[ , name[2] ]  ;  if(missing(xlab)) xlab <- name[2]
    y <- data[ , name[1] ]  ;  if(missing(ylab)) ylab <- name[1]
    if(missing(main)) main <- paste("exponential regression of",
                                    deparse(substitute(data)))
    } else
    {                   # get x and y from formula directly
    name <- as.character(x)[-1]
    x <- get(name[2]) ;  if(missing(xlab)) xlab <- name[2]
    y <- get(name[1]) ;  if(missing(ylab)) ylab <- name[1]
    }
  }
if(is.null(y)) stop("y cannot be NULL. formula statement might be wrong.")
x <- x[y!=0]
y <- y[y!=0]
y <- log10(y)
# Model:
mod <- lm( y ~ x )
# expand digits vector, if necessary
digits <- rep(digits, length.out=4)
# Prepare formula writing
a <- round( coef(mod)[2] , digits[1])
b <- round( coef(mod)[1] , digits[2])
Txt <- paste0("y = 10^(", a, " * x ", ifelse(b>0," + ", " - "), abs(b), ")\nR\U00B2 = ",
          signif(rsquare(x,y), digits[3]), "\nRMSE = ", signif(rmse(x,y), digits[4]))
# Acutal plotting:
if(!logy) y <- 10^y
if(plot){
# make new plot if add is FALSE (the default):
if (!add) plot(x, y, pch=pch, xlab=xlab, ylab=ylab, main=main, yaxt="n",
               xlim=xlim, ylim=ylim, if(logy) type="n", col=col, ...)
# logAxis:
if(logy)
  {
  logAxis(2)
  points(x, y, las=1, pch=pch, col=col, ...)
  } else axis(2, las=1)
# Model line
lx <- seq(par("usr")[1], par("usr")[2], length.out=100)
ly <- predict(mod, newdata=data.frame(x=lx), interval="confidence" )
if(!logy) ly <- 10^ly
# confidence interval band
polygon(x=c(lx, rev(lx)), y=c(ly[,2], rev(ly[,3])), col=rgb(0.3,0.5,0, 0.5))
# prediction line
lines(lx, ly[,1], col=modcol, lwd=lwd, ...)
# write formula
legend(pos1, pos2, Txt, bty="n", text.col=modcol, inset=inset, xpd=xpd)
} # end if plot
# return output if wanted:
if(!is.null(predictnew))
   10^as.numeric(predict(mod, newdata=data.frame(x=predictnew), interval=interval))
} # end of function
