\name{addAlpha}
\alias{addAlpha}
\title{Make existing colors transparent}
\description{Make existing colors semi-transparent (add alpha)}
\usage{addAlpha(col,alpha = 1) }
\arguments{
  \item{col}{vector of color names (\code{\link{colors}}) that can be interpreted by \code{\link{col2rgb}}}
  \item{alpha}{Level of (semi-)transparency. between 0 (transparent) and 1 (intransparent). DEFAULT: 1}
}
\value{character vector with hexadecimal color codes.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, June 2014
as suggested by Mathias Seibert, Dec. 2013}
\seealso{\code{\link{rgb}}, \code{\link{colors}}, \code{\link{col2rgb}}}
\examples{
addAlpha("red", c(0.1, 0.3, 0.6, 1))
NewColors <- addAlpha(c("red","blue","yellow","green", "purple"), 0:200/200)
plot(runif(1000), col=NewColors, pch=16, cex=2)
}
\keyword{dplot}
\keyword{color}
