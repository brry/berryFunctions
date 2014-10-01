\name{quantileBands}
\alias{quantileBands}
\title{quantile bands}
\description{Quantile bands with optional smoothing, e.g. for visualizing simulations}
\usage{quantileBands(mat, x=1:ncol(mat), col=rgb(0,0,1, alpha=c(0.5, 0.7)),
add=FALSE, main="Quantile Bands", ylab="", xlab="", probs=0:4/4, na.rm=FALSE,
type=7, smooth=NA, medargs=NULL, meanargs=NULL, txi, textargs=NULL, ...)}
\arguments{
  \item{mat}{Matrix or data.frame with columns of data}
  \item{x}{X-axis positions for each column. DEFAULT: 1:ncol(mat)}
  \item{col}{Vector of colors for each quantile group, recycled reversively if necessary. DEFAULT: rgb(0,0,1, alpha=c(0.5, 0.7))}
  \item{add}{Add to existing plot? Allows to add to highly customized plot. DEFAULT: FALSE}
  \item{main, xlab, ylab}{plot labels. DEFAULT: "Quantile Bands", ""}
  \item{probs}{Probabilities passed to \code{\link{quantile}}. DEFAULT: 0:4/4}
  \item{na.rm}{Remove NAs before computing \code{\link{quantile}s}, \code{\link{median}} and \code{\link{mean}}? DEFAULT: FALSE}
  \item{type}{Which of the 9 \code{\link{quantile}} algorithms should be used. DEFAULT: 7}
  \item{smooth}{If(!is.na), \code{width} passed to \code{\link{movAv}} smoothing quantiles. DEFAULT: NA}
  \item{medargs}{List of arguments passed to lines drawing \code{\link{median}}. Not drawn if NULL. DEFAULT: NULL}
  \item{meanargs}{List of arguments passed to lines drawing \code{\link{mean}}. Not drawn if NULL. DEFAULT: NULL}
  \item{txi}{Text x position index (along columns of mat), recyled if necessary. NA to suppress. INTERNAL DEFAULT: middle of the plot for all.}
  \item{textargs}{List of arguments passed to \code{\link{text}}, like col, adj, ... DEFAULT: NULL}
  \item{\dots}{Further arguments passed to \code{\link{polygon}}, like border, lty, ...}
}
\value{Quantiles of each column, invisible. Smoothed if \code{smooth} is given!}
\note{This is the first version and is not tested very well yet.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Sept 2014}
\seealso{\code{\link{quantile}}, \code{\link{quantileMean}}, \code{\link{polygon}}}
\examples{
neff <- t(sapply(1:30, function(i) sapply(1:100*10, function(n) max(rnorm(n)))))
qB <- quantileBands(neff, x=1:100*10)
qB[,1:10]
quantileBands(neff, x=1:100*10, smooth=19, meanargs=list(col=2), txi=NA)

library(RColorBrewer)

quantileBands(neff, x=1:100*10, smooth=9, ylab="max of rnorm(n)",
  xlab="sample size (n)", probs=0:10/10, col=brewer.pal(5,"BuGn"),
  medargs=list(lwd=2), meanargs=list(col=2, lty=1), txi=c(40,50,60),
  main="Maximum is an unsaturated statistic:\n it rises with sample size")

neff2 <- t(sapply(1:30, function(i) sapply(1:100*10, function(n) mean(rnorm(n)))))
quantileBands(neff2, x=1:100*10, smooth=9, ylab="mean of rnorm(n)",
  xlab="sample size (n)", probs=0:10/10, col=brewer.pal(5,"BuGn"),
  txi=c(40,50,60), textargs=list(col="yellow"), medargs=list(lwd=2),
  meanargs=list(col=2, lty=1), main="Mean converges to true population mean")

}
\keyword{dplot}
\keyword{aplot}
\keyword{univar}
