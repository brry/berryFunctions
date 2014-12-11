\name{linReg}
\alias{linReg}
\title{linear regression with plotting}
\description{uses \code{\link{lm}}; plots data if add=FALSE, draws the regression line with \code{\link{abline}} and writes the formula with \code{\link{legend}}}
\usage{linReg(x, y=NULL, data=NULL, digits=2, inset=0, xpd=par("xpd"), pos1="top",
pos2=NULL, add=FALSE, pch=16, col=2, lwd=1, xlab=deparse(substitute(x)),
ylab=deparse(substitute(y)), main="linear regression", ...)}
\arguments{
  \item{x}{Numeric or formula (see examples). Vector with values of explanatory variable}
  \item{y}{Numeric. Vector with values of dependent variable. DEFAULT: NULL}
  \item{data}{Dataframe. If x is a formula, the according columns from data are used as x and y. DEFAULT: NULL}
  \item{digits}{Numeric vector of length \eqn{\ge 1}. Specifies number of digits a,b,r,e are rounded to in the formula "y=a*x+b \\n R^2=r \\n RMSE=e", respectively. If values are not specified, they are set equal to the first. DEFAULT: 2}
  \item{inset}{Numeric vector of length \eqn{\le 2}. inset distance(s) from the margins as a fraction of the plot region when formula is placed by keyword. DEFAULT: 0}
  \item{xpd}{Logical, specifying wheter formula can be written only inside the plot region (when FALSE) or inside the figure region including mar (when TRUE) or in the entire device region including oma (when NA). DEFAULT: par("xpd")}
  \item{pos1}{\code{\link{xy.coords}}-acceptable position of the formula. DEFAULT: "top"}
  \item{pos2}{For numerical coordinates, this is the y-position. DEFAULT: NULL, as in \code{\link{legend}}}
  \item{add}{Logical. If TRUE, line and text are added to the existing graphic. DEFAULT: FALSE (plots datapoints first and then the line.)}
  \item{pch}{Point Character, see \code{\link{par}}. DEFAULT: 16}
  \item{col}{Color, see \code{\link{par}}. DEFAULT: 2}
  \item{lwd}{Numeric. Linewidth, see \code{\link{par}}. DEFAULT: 1}
  \item{xlab, ylab, main}{Character / Expression. axis label and graph title if add=FAlSE. DEFAULT: deparse(substitute(x)) or y, "linear regression"}
  \item{\dots}{Further arguments passed to plot and abline.}
}
\value{None, used for plotting and drawing.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2011-2012}
\seealso{ \code{\link{lm}}, \code{\link{mReg}}, \code{\link{expReg}}, \code{\link{legend}}, \code{\link{par}}, \code{\link{abline}}. }
\examples{

a <- 1:30
b <- a/2.345+rnorm(30,0,3)

linReg(a,b)
linReg(a,b, ylab="Hallo", pch=1, col=3, main="Regression by Berry")
linReg(a, b, pos1=15, pos2=0) # topleft corner of legend

# For more flexibility with the datapoints, plot first, then use linReg with add=TRUE:
plot(a,b, xlim=c(-5,45))
linReg(a, b, pos1="bottomright", add=TRUE, inset=.1) # inset: distance from plot border
linReg(a, b, digits=c(7,4,3), add=TRUE, col=3, lty=2, lwd=4)
linReg(a, b, digits=3, pos1="topleft", inset=c(-0.1, 0.3), xpd=TRUE, add=TRUE)

# Formula specification:
linReg(Volume~Height, data=trees)
linReg(b~a)

}
\keyword{ hplot }
\keyword{ aplot }
\keyword{ regression }
