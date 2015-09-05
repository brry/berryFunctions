# Produce logarithmic interval borders
# Berry Boessenkool, berry-b@gmx.de, Oct 2014
logSpaced <- function(
base=1.1708, # Base for calculations, can be a vector to compare several bases
n=20, # Number of values to be calculated
min=1, max=n, # Range where n values are to be distributed, single values each.
plot=TRUE, # Should the points be plotted on a line?
pch=3, las=1, # PointCharacter and Label Axis Style
ylab="base", # Y axis label
... # Further arguments passed to \code{\link{plot}}
)
{
# input control:
min <- min[1] # in case someone tries to vectorize these
max <- max[1]
n <- n[1]
if(!is.numeric(base)) stop("base must be numeric.")
if(!is.vector(base)) stop("base must be a vector.")
base <- sort(unique(base))
# calculate values (x)
x <- base[1]^(1:n-1)
# map to min-max:
if(all(x==1)) x <- 1:n # if base=1
lmc <- coef(lm(c(min,max)~x[c(1,n)]))
x <- lmc[2]*x + lmc[1]
# plot first value:
if(plot) plot(x, rep(base[1],n), ylim=range(base), pch=pch, las=las, ylab=ylab, ...)
# for all other base values, if this is a vector:
if(length(base)==1) return(x) else
  {
  x <- sapply(base, function(y) y^(1:n-1))
  colnames(x) <- base
  for(i in 1:ncol(x))
    {
    if(all(x[,i]==1)) x[,i] <- 1:n
    lmc <- coef(lm(c(min,max)~x[c(1,n),i]))
    x[,i] <- lmc[2]*x[,i] + lmc[1]
    }
  if(plot) for(i in 2:ncol(x)) points(x[,i], rep(base[i],n), pch=pch)
  return(x)
  }
}

