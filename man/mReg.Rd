\name{mReg}
\alias{mReg}
\title{multiple regression}
\description{
Multiple regression fitting various function types including e.g. linear, cubic, logarithmic, exponential, power, reciprocal.
Quick way to find out what function type fits the data best.
Plots data and fitted functions and adds a legend with the functions (or their types=structure) sorted by R squared.
Returns the fitted functions with their parameters and R^2 values in a data.frame.}
\usage{mReg(x, y, Poly45=FALSE, exp_4=FALSE,
xf=deparse(substitute(x)),  yf=deparse(substitute(y)), ncolumns=9, plot=TRUE,
add=FALSE,  nbest=12, R2min, selection=NULL, digits=2, extend=0.4,
xlim=range(x, finite=TRUE) + c(-1,1)* extend * diff(range(x,  finite=TRUE)),
ylim=range(y, finite=TRUE) + c(-1,1)* extend * diff(range(y, finite=TRUE)),
xlab=xf,  ylab=yf, las=1, lwd=rep(1, 12), lty=rep(1, 12), col=NULL,
pcol=par("col"), pch=16, legend=TRUE, legargs=NULL, legendform="nameform", ...)}
\arguments{
  \item{x}{Numerical vector. x coordinates of points for regression}
  \item{y}{Y-values}
  \item{Poly45}{Logical. Should 4th and 5th degree polynomials also be fitted? DEFAULT: FALSE, as the formulas are very long.}
  \item{exp_4}{Logical. Return 4-parametric exponential distibution fits? (only best fit is plotted). exp_4par_ini has the initial values of exponential fitting with the data relocated to first quadrant. The others are optimized with the methods of \code{\link{optim}}. DEFAULT: FALSE}
  \item{xf}{Character. x name for Formula. DEFAULT: substitute(x) before replacing zeros in x and y}
  \item{yf}{Ditto for y}
  \item{ncolumns}{Number of columns in output. Set lower to avoid overcrowding the console. DEFAULT: 9}
  \item{plot}{Logical. plot data and fitted functions? DEFAULT: TRUE}
  \item{add}{Logical. add lines to existing plot? DEFAULT: FALSE}
  \item{nbest}{Integer. Number of best fitting functions to be plotted (console output table always has all). DEFAULT: 12}
  \item{R2min}{Numerical. Minimum Rsquared value for function type to be plotted. Suggestion: 0.6 (2/3 of variation of y is explained by function of x). DEFAULT: empty}
  \item{selection}{Integers of functions to be plotted, assigned as in list in section "note". DEFAULT: NULL, meaning all}
  \item{digits}{Integer. number of significant digits used for rounding formula parameters and R^2 displayed. DEFAULT: 2}
  \item{extend}{Numerical. Extention of axis ranges (proportion of range). DEFAULT: 0.4}
  \item{xlim}{Numerical vector with two values, defining the x-range of the lines to be plotted.  DEFAULT: extended range(x)}
  \item{ylim}{Ditto for Y-axis}
  \item{xlab}{Character. default labels for axis labeling and for formulas. DEFAULT: substitute(x) before replacing zeros in x and y}
  \item{ylab}{Ditto for y axis.}
  \item{las}{Integer in 0:4. label axis style. See \code{\link{par}}. DEFAULT: 1}
  \item{lwd}{Numerical of length 12. line width for lines. DEFAULT: rep(1,12)}
  \item{lty}{Numerical of length 12. line type. DEFAULT: rep(1,12)}
  \item{col}{Numerical of length 12. line colors. DEFAULT: NULL, means they are specified internally}
  \item{pcol}{Numerical. Color used for the data-points themselves. DEFAULT: par('col')}
  \item{pch}{Integer or single character. Point CHaracter for the data points. See \code{\link{par}}. DEFAULT: 16}
  \item{legend}{Logical. add legend to plot? DEFAULT: TRUE}
  \item{legargs}{List. Arguments passed to \code{\link{legend}}. Will overwrite defaults. DEFAULT: NULL}
  \item{legendform}{One of 'full', 'form', 'nameform' or 'name'. Complexity (and length) of legend in plot. See Details. DEFAULT: 'nameform'}
  \item{\dots}{Further graphical parameters passed to plot}
}
\details{
legendform : example\cr
full       : 7.8*x + 6.31\cr
form       : a*x+b\cr
nameform   : linear  a*x+b\cr
name       : linear\cr\cr
full can be quite long, especially with Poly45=TRUE!
}
\value{
data.frame with rounded R squared, formulas, and full R^2 and parameters for further use.
Rownames are the names (types) of function.
Sorted decreasingly by R^2}
\section{warning}{
A well fitting function does NOT imply correct causation!\cr
A good fit does NOT mean that you describe the bahaviour of a system adequatly!\cr
Extrapolation can be DANGEROUS!\cr
Always extrapolate to see if a function fits the expected results there as well.\cr
Avoid overfitting: Poly45 will often yield good results (in terms of R^2),
but can be way overfitted. And outside the range of values, they act wildly.
}
\note{
If you're adjusting the appearance (lwd, lty, col) of single lines, set parameters in the following order:\cr
#  1 linear                 a*x + b\cr
#  2 quadratic (parabola)   a*x^2 + b*x + c\cr
#  3 kubic                  a*x^3 + b*x^2 + c*x + d\cr
#  4 Polynom 4th degree     a*x^4 + b*x^3 + c*x^2 + d*x + e\cr
#  5 Polynom 5              a*x^5 + b*x^4 + c*x^3 + d*x^2 + e*x + f \cr
#  6 logarithmic            a*log(x) + b \cr
#  7 exponential            a*e^(b*x) \cr
#  8 power/root             a*x^b  \cr
#  9 reciprocal             a/x + b \cr
# 10 rational               1 / (a*x + b) \cr
# 11 exponential 4 Param    a*e^(b*(x+c)) + d  \cr
Replaces zeros with 0.00000000000001 for logarithms. Which may put too much weight on these data points - further testing needed!\cr
Negative values are not used for regressions containing logarithms; with warning.\cr
exp_4par was originally developed for exponential temperature decline in a cup of hot water.
}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, December 2012}
\references{Listed here: \url{http://rclickhandbuch.wordpress.com/berryfunctions/#mReg}}
\seealso{\code{\link{glm}}, \code{\link{lm}}, \code{\link{optim}}}

\examples{
set.seed(12)
x <- c(runif(100,0,3), runif(200, 3, 25)) # random from uniform distribution
y <- 12.367*log10(x)+7.603+rnorm(300)     # random from normal distribution
plot(x,y, xlim=c(0,40))
mReg(x,y) # Warnings come from negative y-values

# Passing arguments to legend:
mReg(x,y, pch=1, legargs=list(x="bottomright", cex=0.7), legendform="form")

mReg(x,y, extend=0.2) # less empty space around data points
mReg(x,y, nbest=4) # only 4 distributions plotted
mReg(x,y, legargs=list(x=7, y=8, bty="o")) # Legend position as coordinates

\dontrun{
View(mReg(x,y, Poly45=TRUE, exp_4=TRUE, plot=FALSE)) # exp_4: fit more distributions
}
# optim methods often yield different results, so be careful using this.
# I might insert a possibility to specify initial values for optim.
# 4 Parameters allow several combinations to yield similarly good results!
plot( 0:10, 3.5*exp(0.8*( 0:10 + 2      )) + 15 , type="l")
lines(0:10,  18*exp(0.8*( 0:10 - 2.5e-05)) - 5, col=2)


# okay, different dataset:
x <- c(1.3, 1.6, 2.1, 2.9, 4.4, 5.7, 6.6, 8.3, 8.6, 9.5)
y <- c(8.6, 7.9, 6.6, 5.6, 4.3, 3.7, 3.2, 2.5, 2.5, 2.2)
mReg(x,y, legargs=list(cex=0.7, x="topright"), main="dangers of extrapolation")
points(x,y, cex=2, lwd=2)
# Polynomial fits are good within the data range, but, in this case obviously,
# be really careful extrapolating! If you know that further data will also be low,
# add another point to test differences:
mReg(c(x,11,13,15), c(y,2,2,2), xf="myX", yf="myY", Poly45=TRUE, legendform="name")
points(x,y, cex=2, lwd=2)
# The Polynomials are still very good: they have 5 to 6 Parameters, after all!
# Poly45 is set to FALSE by default to avoid such overfitting.
 
mReg(x,y, pcol=8, ncol=0) # no return to console

# only plot a subset: best n fits, minimum fit quality, or user selection
mReg(x,y, pcol=8, ncol=2, nbest=4)
mReg(x,y, pcol=8, ncol=2, R2min=0.7)
mReg(x,y, pcol=8, ncol=2, selection=c(2,5,8))
# selecting the fifth degree polynomial activates Poly45 (in the output table)

# Add to axisting plot:
plot(x,y, xlim=c(0,40))
mReg(x,y, add=TRUE, lwd=12:1/2, ncol=0)
# lwd, lty can be vectors of length 12, specifying each line separately.
# Give those in fix order (see section notes), not in best-fit order of the legend.
# The order is Polynomial(1:5), log, exp, power, reciprocal, rational, exp_4_param
# color has to be a vector of 12
# opposedly, lwd and lty are repeated 12 times, if only one value is given


# One more dataset:
j <- c(5,8,10,9,13,6,2) ; k <- c(567,543,587,601,596,533,512)
# Inset from margin of plot region:
mReg(j,k, legargs=list(x="bottomright", inset=.05, bty="o"), legendform="name")
# Legend forms
mReg(j,k, legargs=list(x="bottomright"), legendform="name")
mReg(j,k, legargs=list(x="bottomright"), legendform="form")
mReg(j,k, legargs=list(x="bottomright"), legendform="nameform")
mReg(j,k, legargs=list(x="bottomright"), legendform="full")


# The question that got me started on this whole function...
# exponential decline of temperature of a mug of hot chocolate
tfile <- system.file("extdata/Temp.txt", package="berryFunctions")
temp <- read.table(tfile, header=TRUE, dec=",")
head(temp)
plot(temp)
temp <- temp[-20,] # missing value - rmse would complain about it

x <- temp$Minuten
y <- temp$Temp 
mReg(x,y, exp_4=TRUE, selection=11)
# y=49*e^(-0.031*(x - 0  )) + 25 correct, judged from the model:
# Temp=T0 - Te *exp(k*t) + Te     with    T0=73.76,  Tend=26.21, k=-0.031
# optmethod="Nelder-Mead"  # y=52*e^(-0.031*(x + 3.4)) + 26 wrong


x <- seq(1, 1000, 1)
y <- (x+22)/(x+123) # can't find an analytical solution so far. Want to check out nls
mReg(x, y, posx="right")


# Solitaire Results. According to en.wikipedia.org/wiki/Klondike_(solitaire):
# Points=700000/Time + Score
# I recorded my results on a windows XP system in 2010 as an excuse to play this game a lot.
# Games longer than 100 seconds are intentionally bad to find what happens then...
sfile <- system.file("extdata/solitaire.txt", package="berryFunctions")
solitaire <- read.table(sfile, header=TRUE)
mReg(solitaire$Time, solitaire$Points) # and yes, reciprocal ranks highest! Play Fast!

# multivariate regression should be added, too:
sfile <- system.file("extdata/gelman_equation_search.txt", package="berryFunctions")
mv <- read.table(sfile, header=TRUE)
}
\keyword{aplot}
\keyword{hplot}
\keyword{regression}
\keyword{nonlinear}
\keyword{multivariate}

