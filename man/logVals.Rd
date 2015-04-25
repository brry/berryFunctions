\name{logVals}
\alias{logVals}
\title{Create log-axis values and labels}
\description{create nice values and labels to write at logartihmic axes}
\usage{logVals(from=-7, to=7, Range, base=1, big.mark="'", decimal.mark=".",
scientific=FALSE, exponent=Inf, expobase1=FALSE, allbase=1:9, ...)}
\arguments{
  \item{from}{Lower exponent \emph{OR} vector with data}
  \item{to}{High end}
  \item{Range}{Or give from and to as range}
  \item{base}{Bases to be used, eg. c(1,2,5)}
  \item{big.mark}{Symbol separating thousands, eg. space, comma, dot, etc. see \code{\link{format}} and \code{\link{prettyNum}}}
  \item{decimal.mark}{Character separating comma values, see \code{\link{format}} and \code{\link{prettyNum}}}
  \item{scientific}{See \code{\link{format}}}
  \item{exponent}{Starting at which exponent should \code{labs} be an expression with exponents? Compare to \code{\link{options}("scipen")}. This is mainly for \code{\link{logAxis}} and only for base 1. DEFAULT: Inf}
  \item{expobase1}{Should "n * " be appended before 10^exp if n=1? DEFAULT: FALSE}
  \item{allbase}{Base for \code{$all} (for horizontal lines). DEFAULT: 1:9}
  \item{\dots}{Ignored arguments}
}
\value{A list with
  \item{vals}{Values for lines and label positions}
  \item{labs}{Formatted values for labels}
  \item{all}{Values for lines}
}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Feb 2014}
\seealso{ \code{\link{log10}}, \code{\link{logAxis}}, \url{http://r.789695.n4.nabble.com/expression-exponent-labeling-td4661174.html}}
\examples{
# Easiest use: vector with data (logVals automatically finds range):
y <- 10^runif(50, -1, 2)
plot(y, log="y") # not much control over placement and format of labels
plot(y, log="y", yaxt="n")
# now do this better, with custom bases:
lv <- logVals(y, base=c(1,2,5) )
axis(2, lv$vals, lv$labs, las=1)

# Default arguments:
lv <- logVals()
str(lv) # values, formatted labels, all 10^x values for lines
plot(1, ylim=c(1e-3, 1e4), log="y", yaxt="n", yaxs="i")
abline(h=lv$all, col=8 )
box("plot")
axis(2, lv$vals, lv$labs, las=1)
lines(seq(0.5, 1.5, len=50), 10^runif(50, -3, 4), col=2)

# Formatting labels:
logVals(                )$labs
logVals(scient=TRUE     )$labs
logVals(exponent=5      )$labs # expression with exponent, see logAxis
logVals(big.mark=" "    )$labs
logVals(big=".", dec=",")$labs # German style (not recommended)
}
\keyword{ aplot }
\keyword{ dplot }
