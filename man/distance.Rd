\name{distance}
\alias{distance}
\title{calculate distance between points}
\description{calculate distance between points on planar surface}
\usage{distance(xpt, ypt, xref, yref)}
\arguments{
  \item{xpt}{vector with x-coordinate(s) of point(s)}
  \item{ypt}{ditto for y}
  \item{xref}{single x coordinate of reference point}
  \item{yref}{ditto for y}
}
\details{The function is quite simple:
\code{sqrt((xref - xpt)^2 + (yref - ypt)^2)}}
\value{vector with the distances}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2012}
\seealso{\code{\link[spatstat]{nndist}} in the package \code{spatstat} for distance to nearest neighbour}
\examples{
A <- c(3,  9,-1)
B <- c(7, -2, 4)
plot(A,B)
text(A,B, paste0("P",1:3), adj=1.1)
points(3,5, col=2, pch=16)
segments(3,5, A,B)
distance(A,B, 3,5)
text(c(3.2,6,1), c(6,1,4), round(distance(A,B, 3,5),2) )
}
\keyword{spatial}

