\name{owa}
\alias{owa}
\title{overwrite argument default lists}
\description{Expansion of ellipsis (three dots).
Used in functions that pass argument lists separately to several functions.
Internal defaults can be set per function (eg. one list for plot and one for legend).
Some of the defaults can be overwritten, some should be left unchanged, some can be
additionally specified by users. owa combines everything accordingly.
See the example section on how to implement this.}
\usage{owa(d, a, ...)}
\arguments{
  \item{d}{Default arguments}
  \item{a}{Arguments specified by user}
  \item{\ldots}{names of unchangeable arguments (that will not be overwritten) as character strings.}
}
\note{the argument u has been replaced by ellipsis (...) in version 1.7 (Dec. 2014)!}
\value{Always a list, disregarding list/vector mode of input}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Early 2014}
\references{
   \url{http://stackoverflow.com/questions/3057341}\cr
   \url{http://stackoverflow.com/questions/5890576}\cr
   \url{http://stackoverflow.com/questions/4124900}\cr
   \url{http://stackoverflow.com/questions/16774946}\cr}
\examples{
d <- list(bb=1:5, lwd=2, lty=1,   col="gray")
a <- list(bb=3,   lwd=5, lty="1", wachs="A", bb=16)

owa(d, a, "bb", "lwd") # basic usage: lty is overwritten
owa(d, NULL, "bb", "wachs") # NULL is a good default for argument lists
owa(d, c(HH=2, BBB=3) ) # vectors and lists are all converted to lists
owa(d, list(lwd=5, bb=3, lty="1") ) # order of arguments doesn't matter

# Usage example (see application eg. in funnelPlot, colPoints or mReg)

# Why we want to do this:
testfun <- function(...) {plot(7:9, ...) ; legend("top", "Text hier", ...)}
testfun()
# testfun(type="o") # Error: legend doesn't have the argument 'type'!


# How to solve this:
testfun <- function(data=7:9, legarg=NULL, plotarg=NULL)
   {
   # defaults for plot and legend:
   plot_def <- list(x=0.5*data, col="red", cex=2, lty=2, type="o")
   leg_def <- list(x="top", lty=2, legend="Default text here")
   # combine defaults and user specified into final argument list
   plot_fin <- owa(d=plot_def, a=plotarg, "col", "lty")
   leg_fin <- owa(d=leg_def, a=legarg, "lty")
   # Execute single functions that each have their own arguments:
   do.call(  plot, args=plot_fin)
   do.call(legend, args=leg_fin)
   }

testfun()
testfun(plotarg=list(type="l", col="blue") )
# color is silently ignored, as it is defined as unchangeable
testfun(plotarg=list(type="l"), legarg=list(col="blue", pch=16) )

}
\keyword{programming}
