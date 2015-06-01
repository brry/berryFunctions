\name{movAvLines}
\alias{movAvLines}
\title{moving average with different window widths}
\description{adds moving average lines with different window widths to a plot}
\usage{movAvLines(y, x=1:length(y), widths=2:7*2-1, weights,
col="blue", alpha=0.3, plot=FALSE, las=1, ... )}
\arguments{
  \item{y}{y values that are smoothed with several window widths}
  \item{x}{x values of data. DEFAULT: 1:length(y)}
  \item{widths}{widths of \code{\link{movAv}} windows. DEFAULT: 2:7*2-1}
  \item{weights}{weights within each window}
  \item{col}{color passed to \code{\link{addAlpha}}. DEFAULT: "blue"}
  \item{alpha}{transparency passed to \code{\link{addAlpha}}. DEFAULT: 0.3}
  \item{plot}{should scatterplot be created first? DEFAULT: FALSE}
  \item{las}{LabelAxisStyle (only relevant if plot=TRUE). DEFAULT: 1}
  \item{\dots}{further arguments passed to \code{\link{lines}}}
}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, May 2015}
\seealso{\code{\link{movAv}}, \code{\link{addAlpha}} }
\examples{
set.seed(42)
movAvLines(cumsum(rnorm(50)), plot=TRUE, lwd=3)
}
\keyword{ts}
\keyword{manip}
\keyword{smooth}
