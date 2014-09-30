\name{randomPoints}
\alias{randomPoints}
\title{distanced random points}
\description{arranges points in square randomly, but with certain minimal distance to each other}
\usage{randomPoints(xmin, xmax, ymin, ymax, number, mindist, plot=TRUE, ...)}
\arguments{
  \item{xmin}{Minimum x coordinate}
  \item{xmax}{Upper limit x values}
  \item{ymin}{Ditto for y}
  \item{ymax}{And yet again: Ditto.}
  \item{number}{How many points should be randomly + uniformly distributed}
  \item{mindist}{Minimum DIstance each point should have to others}
  \item{plot}{Plot the result? DEFAULT: TRUE}
  \item{\dots}{Further arguments passed to plot}
}
\value{data.frame with x and y coordinates.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2011/2012}

\seealso{\code{\link{distance}}}
\examples{
P <- randomPoints(xmin=200,xmax=700, ymin=300,ymax=680, number=60,mindist=10, asp=1)
rect(xleft=200, ybottom=300, xright=700, ytop=680, col=NA, border=1)

format( round(P,4), trim=FALSE)

\dontrun{windows(record=TRUE)}
for(i in 1:10)
{
rp <- randomPoints(xmin=0,xmax=20, ymin=0,ymax=20, number=20, mindist=3, plot=FALSE)
plot(rp, las=1, asp=1, pch=16)
abline(h=0:30*2, v=0:30*2, col=8); box()
for(i in 1:nrow(rp))
    circle(rp$x[i],rp$y[i], r=3, col=rgb(1,0,0,alpha=0.2), border=NA)
}

}
\keyword{datagen}
\keyword{spatial}
