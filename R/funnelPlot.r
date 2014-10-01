# Funnel plots for proportional data. Inspired by Stephen Few
# implemented in R by Berry Boessenkool    berry-b@gmx.de    2013-10-12
# Please contact me if you know a good package for this function!
# And also if you find bugs - I didn't yet test everything thoroughly.

# This is the first time I use seperate argument lists for different functions (lines, abline, points, text, legend).
# I'd love to get some feedback on this - is it comprehensible with the examples?
# Are there more elegant ways to do this?


# Function ---------------------------------------------------------------------
funnelPlot <- function(
                       x,  # number of successes (cases)
                       n,  # number of trials (population)
                       labels=NULL, # Labels for points
                       method="classic", # see notes. can also be "wilson"
                       add=FALSE, # add to exisiting plot instead of drawing new plot?
                       xlim=range(n, finite=TRUE), # graphical parameters
                       ylim=range(x/n*100, finite=TRUE), # always in %!
                       las=1,
                       xlab="Sample size n",
                       ylab="Success rate [%]",
                       main="Funnel plot for Proportions",
                       a3=NULL, # list with arguments for CI lines at 3*sd (eg: col, lty, lwd, lend, etc.)
                       # overwrites defaults that are defined within the function (if contentually possible)
                       a2=NULL, # for line of 2 sd
                       am=NULL, # arguments for mean line
                       ap=NULL, # for the data points (cex, etc.)
                       at=NULL, # for text (labels of each point)
                       al=NULL, # for legend  (text.col, bty, border, y.intersp, etc.)
                       ...) # further arguments passed to plot only!
{
# Data (proportions) -----------------------------------------------------------
p <- x/n*100 # p: proportion of success
m <- mean(p, na.rm=TRUE) # m: mean value of proportions
# Distribute line point values with high density at rapidly changing curves
nl <- 10^seq(log10(xlim[1]), log10(xlim[2]), len=500) # nl: n for ci-lines
# calculate CI  (2*sd and 3*sd limits) -----------------------------------------
# f: factor for confidence interval
f1 <- qnorm(1-0.05/2) # 1.959964   1.96    # 0.025 = 2.5%    alpha=0.95
f2 <- qnorm(1-0.002/2) # 3.090232          # 0.001 = 0.1%    alpha=0.998
if(method=="wilsonapho")
ci <- data.frame(l2sigma=(2*nl*m/100+f1^2 - f1*sqrt(f1^2+4*nl*m/100*(1-m/100)))/(nl+f1^2)/2,
                 u2sigma=(2*nl*m/100+f1^2 + f1*sqrt(f1^2+4*nl*m/100*(1-m/100)))/(nl+f1^2)/2,
                 l3sigma=(2*nl*m/100+f2^2 - f2*sqrt(f2^2+4*nl*m/100*(1-m/100)))/(nl+f2^2)/2,
                 u3sigma=(2*nl*m/100+f2^2 + f2*sqrt(f2^2+4*nl*m/100*(1-m/100)))/(nl+f2^2)/2 )*100
else
if(method=="wilson")
ci <- data.frame(l2sigma=1/(1+f1^2/nl)*(m/100+f1^2/2/nl - f1*sqrt(m/100*(1-m/100)/nl+f1^2/4/nl^2)),
                 u2sigma=1/(1+f1^2/nl)*(m/100+f1^2/2/nl + f1*sqrt(m/100*(1-m/100)/nl+f1^2/4/nl^2)),
                 l3sigma=1/(1+f2^2/nl)*(m/100+f2^2/2/nl - f2*sqrt(m/100*(1-m/100)/nl+f2^2/4/nl^2)),
                 u3sigma=1/(1+f2^2/nl)*(m/100+f2^2/2/nl + f2*sqrt(m/100*(1-m/100)/nl+f2^2/4/nl^2)) )*100
else
if(method=="classic")
ci <- data.frame(l2sigma= m + f1*sqrt( m*(100-m) / nl ),
                 u2sigma= m - f1*sqrt( m*(100-m) / nl ),
                 l3sigma= m + f2*sqrt( m*(100-m) / nl ),
                 u3sigma= m - f2*sqrt( m*(100-m) / nl ))
else stop("Wrong method. Possible are 'classic' and 'wilson'")
#
# Plot preparation -------------------------------------------------------------
# default arguments (da)
da3 <- list(x=nl, col="black", lty=1, lwd=par("lwd")) # da_: default arguments for _lines at 3*sd
da2 <- list(x=nl, col="gray",  lty=1, lwd=par("lwd")) # lines 2sd
dam <- list(h=m, col="orange", lty=1, lwd=par("lwd")) # abline mean
dap <- list(x=n, y=p, col="orange", pch=16)  # points
dat <- list(x=n, y=p, labels=labels, adj=c(0,1), col="black") # text (labels)
# final arguments for lines (so they can also be used by legend default arguments)
fa3 <- owa(da3, a3, "y")
fa2 <- owa(da2, a2, "y")
fam <- owa(dam, am, "h")
# default arguments legend:
dal <- list(x="topright", legend=c("3*sd CI (99.8%)","2*sd CI (95%)", "mean"),
            col=c(fa3$col, fa2$col, fam$col),
            lty=c(fa3$lty, fa2$lty, fam$lty),
            lwd=c(fa3$lwd, fa2$lwd, fam$lwd))
#browser()
# Plot everything --------------------------------------------------------------
if(!add) plot(nl, ci[,1], type="n", ylim=ylim, las=las, xlab=xlab, ylab=ylab, main=main, ...)
do.call(abline, args = fam ) # fam: final arguments for mean line drawing
do.call(lines,  args = c(list(y=ci[,1]), fa2) )
do.call(lines,  args = c(list(y=ci[,2]), fa2) )
do.call(lines,  args = c(list(y=ci[,3]), fa3) )
do.call(lines,  args = c(list(y=ci[,4]), fa3) )
do.call(points, args = owa(dap, ap, c("x","y")) )
do.call(text,   args = owa(dat, at) )
do.call(legend, args = owa(dal, al, c("col", "lty", "lwd", "pch")) )
} # End of function ------------------------------------------------------------
