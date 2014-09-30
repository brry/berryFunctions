# Regression with 4-parametric exponential function  a*e^(b*(x+c))+d
# Berry Boessenkool, 2012-2013, outsourced from mReg in July 2014
# berry-b@gmx.de    Any feedback is welcome!

exp4p <- function(
    x, y, # x and y Data
    digits=2, # significant digits for rounding formula output and R^2 in legend
    plot=FALSE,  # plot data and fitted functions?
    las=1, # label axis style, see \code{\link{par}}
    col=1:6, # 6 colors for lines and legend texts
    legarg=NULL, # Arguments passed to \code{\link{legend}}
    ...) # further graphical parameters passed to \code{\link{plot}}
{
# Prepare Output Table
output <- as.data.frame(matrix(NA, ncol=7, nrow=6 ))
colnames(output) <- c("R2","Formulas","R2full", letters[1:4] )
rownames(output) <- c("ini", "N-M", "BFGS", "CG", "SANN", "L--B")
#
# initial parameters via lm of values relocated to first quadrant
init_c <- -min(x, na.rm=TRUE)
init_d <- min(y, na.rm=TRUE)
y_ini <-  y - init_d  + 0.05*abs(init_d)   #; y_ini[y_ini==0] <- 0.001
x_ini <-  x + init_c
init_model <- lm( log(y_ini) ~ x_ini ) # exponential model
init_a <- exp(coef(init_model)[1])
init_b <- coef(init_model)[2]
param <- c(a=init_a, b=init_b, c=init_c, d=init_d)
names(param) <- letters[1:4]
#
# Exponential function to be fitted via optim
expfun <- function(p, x) p[1]*exp(p[2]*(x+p[3]))+p[4]
# function returning one value to be minimized via optim
minfun <- function(p) rmse(y, expfun(p, x=x)) # Root Mean Square Error
#
# Fitting of parameters with different methods
output[1, 4:7] <- param
output[2, 4:7] <- optim(par=param, fn=minfun, method="Nelder-Mead")$par
output[3, 4:7] <- optim(par=param, fn=minfun, method="BFGS")$par
output[4, 4:7] <- optim(par=param, fn=minfun, method="CG")$par
output[5, 4:7] <- optim(par=param, fn=minfun, method="SANN")$par
opt_L      <- try(optim(par=param, fn=minfun, method="L-BFGS-B")$par, silent=TRUE)
for(i in 2:5) if(class(opt_L)=="try-error")
  opt_L<- try(optim(output[i,4:7], fn=minfun, method="L-BFGS-B")$par, silent=TRUE)
if(class(opt_L)=="try-error") opt_L <- list(par=c(a=NA,b=NA,c=NA,d=NA))
output[6, 4:7] <- opt_L
#
# R squared values
output$R2full <- sapply(1:6, function(i) rsquare(y, expfun(as.numeric(output[i, 4:7]), x=x)))
output$R2 <- round(output$R2full, digits)
# descending order of goodness of fit, for legend
output <- output[ order(output$R2full, decreasing=TRUE) , ]
#
# plot data and function fit ------------------------------------------------------
if(plot)
  {
  plot(x, y, las=las, ...)
  xdraw <- seqR(par("usr")[1:2], len=200)
  for(i in 1:6)
  lines(xdraw, output$a[i]*exp(output$b[i]*(xdraw+output$c[i]))+output$d[i], col=col[i])
  do.call(legend, owa(list(x="topright", legend=rownames(output), col=col, lty=1), legarg, "col"))
}
#
# Return Output
output
} # Function end ---------------------------------------------------------------

