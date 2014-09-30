\name{circle}
\alias{circle}
\title{draw circles with a given radius}
\description{draws filled circles with a certain radius using \code{\link{polygon}} and \code{\link{sin}}}
\usage{circle(x, y, r, locnum=100, ...)}
\arguments{
  \item{x}{x coordinate}
  \item{y}{y coordinate}
  \item{r}{radius in units of current plot}
  \item{locnum}{number of calculated points on the circle. DEFAULT: 100}
  \item{\dots}{further arguments passed to polygon}
}
\value{none. Used for drawing.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2012}
\seealso{
\code{\link{polygon}}
}
\examples{
plot(1:10, asp=1)
circle(5,5, r=3)
circle(6,5, r=2, col=2, border=4, lwd=3)

x <- sample(1:20, 15)  ; y <- sample(1:20, 15)
plot(x,y, asp=1) # 1:1 aspect shows they're really circles and not ellipses.
for(i in 1:15) circle(x[i],y[i], r=3, col=rgb(1,0,0,alpha=0.2), border=NA)
}
\keyword{aplot}
