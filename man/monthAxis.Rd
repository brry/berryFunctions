\name{monthAxis}
\alias{monthAxis}
\title{Label date axis}
\description{monthAxis labels date axes at sensible intervals in the time region of weeks to decades.}
\usage{monthAxis(side=1, startyear=NULL, stopyear=NULL, n=5, npm=NULL,
npy=NA, format="\%d.\%m.\\n\%Y", mgp=c(3,1.5,0), cex.axis=1, las=1, ...)}
\arguments{
  \item{side}{Which \code{\link{axis}} are to be labeled? (can be several). DEFAULT: 1}
  \item{startyear}{Integer. starting year. DEFAULT: NULL = internally computed from \code{\link{par}("usr")}}
  \item{stopyear}{Ditto for ending year. DEFAULT: NULL}
  \item{n}{Approximate number of labels that should be printed (as in code{\link{pretty}}). DEFAULT: 5}
  \item{npm}{Number of labels per month, overrides n. DEFAULT: NULL = internally computed.}
  \item{npy}{Number of labels per year, overrides npm and n. DEFAULT: NA}
  \item{format}{Format of date, see details in \code{\link{strptime}}. DEFAULT: "\%d.\%m.\\n\%Y"}
  \item{mgp}{MarGinPlacement, see \code{\link{par}}. DEFAULT: c(3,1.5,0)}
  \item{cex.axis}{CharacterEXpansion (letter size). DEFAULT: 1}
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
monthAxis(1)
monthAxis(1, npm=2) # fix number of labels per month

Date2 <- sort(as.Date("2011-07-13")+sample(0:1400, 50))
plot(Date2, cumsum(rnorm(50)), type="l", xaxt="n", ann=FALSE)
monthAxis(1, format="")
monthAxis(1, npy=6) # fix number of labels per year

Date3 <- sort(as.Date("2011-07-13")+sample(0:1200, 50))
plot(Date3, cumsum(rnorm(50)), type="l", xaxt="n", ann=FALSE)
monthAxis(1, n=4, font=2)
monthAxis(1, col.axis=3) # too many labels with default n=5
}
\keyword{chron}
\keyword{aplot}
\keyword{dplot}
