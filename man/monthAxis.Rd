\name{monthAxis}
\alias{monthAxis}
\title{Label date axis}
\description{monthAxis labels date axes at sensible intervals in the time region of weeks to decades.}
\usage{monthAxis(side=1, startyear=NULL, stopyear=NULL, n=5, npm=NULL,
npy=NA, format="\%d.\%m.\\n\%Y", labels=format.Date(d, format),
midyear=FALSE, mgp=c(3,1.5,0), cex.axis=1, tick=TRUE, las=1, ...)}
\arguments{
  \item{side}{Which \code{\link{axis}} are to be labeled? (can be several). DEFAULT: 1}
  \item{startyear}{Integer. starting year. DEFAULT: NULL = internally computed from \code{\link{par}("usr")}}
  \item{stopyear}{Ditto for ending year. DEFAULT: NULL}
  \item{n}{Approximate number of labels that should be printed (as in code{\link{pretty}}). DEFAULT: 5}
  \item{npm}{Number of labels per month, overrides n. DEFAULT: NULL = internally computed.}
  \item{npy}{Number of labels per year, overrides npm and n. DEFAULT: NA}
  \item{format}{Format of date, see details in \code{\link{strptime}}. DEFAULT: "\%d.\%m.\\n\%Y"}
  \item{labels}{labels. DEFAULT: format.Date(d, format)}
  \item{midyear}{Place labels in the middle of the year? if TRUE, format default is "\%Y". DEFAULT: FALSE}
  \item{mgp}{MarGinPlacement, see \code{\link{par}}. The second value is for label distance to axis. DEFAULT: c(3,1.5,0)}
  \item{cex.axis}{CharacterEXpansion (letter size). DEFAULT: 1}
  \item{tick}{Draw tick lines? DEFAULT: TRUE}
  \item{las}{LabelAxisStyle for orientation of labels. DEFAULT: 1 (upright)}
  \item{\dots}{Further arguments passed to \code{\link{axis}}, like \code{lwd, col.ticks, hadj, lty}, ...}
}
\value{The dates that were labelled}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Feb 2015}
\seealso{\code{\link{monthLabs}} for the numbercrunching itself, \code{\link{axis.Date}} with defaults that are less nice.}
\examples{
set.seed(007) # for reproducibility
Date1 <- sort(as.Date("2013-09-25")+sample(0:150, 30))
plot(Date1, cumsum(rnorm(30)), type="l", xaxt="n", ann=FALSE)
monthAxis(side=1)
monthAxis(1, npm=2, cex.axis=0.5) # fix number of labels per month

plot(Date1, cumsum(rnorm(30)), type="l", xaxt="n", ann=FALSE)
monthAxis(labels=FALSE)
monthAxis(1, format=" ", col.ticks=2)  # equivalent to axis(labels=FALSE)
monthAxis(1)
d <- monthAxis(1, labels=letters[1:24], mgp=c(3,2.5,0))
d # d covers the full year, thus is longer than n=5

Date2 <- sort(as.Date("2011-07-13")+sample(0:1400, 50))
plot(Date2, cumsum(rnorm(50)), type="l", xaxt="n", ann=FALSE)
monthAxis(npy=12, format=" ")  # fix number of labels per year
monthAxis(tcl=-0.8, lwd.ticks=2, format="\%Y/\%m", mgp=c(3,1,0))
monthAxis(format="", mgp=c(3,2,0)) # International Date format YYYY-mm-dd

plot(Date2, cumsum(rnorm(50)), type="l", xaxt="n", ann=FALSE)
monthAxis(midyear=TRUE)

Date3 <- sort(as.Date("2011-07-13")+sample(0:1200, 50))
plot(Date3, cumsum(rnorm(50)), type="l", xaxt="n", ann=FALSE)
monthAxis(1, n=4, font=2)
monthAxis(1, col.axis=3) # too many labels with default n=5

}
\keyword{chron}
\keyword{aplot}
\keyword{dplot}
