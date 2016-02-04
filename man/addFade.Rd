\name{addFade}
\alias{addFade}
\title{color fade out}
\description{Make existing colors fade away}
\usage{addFade(col, fade=0.3, target="white", ... )}
\arguments{
  \item{col}{vector of color names (\code{\link{colors}}), hexadecimal or integer that can be interpreted by \code{\link{col2rgb}}}
  \item{fade}{Level of fading towards target. between 0 (target) and 1 (col). Can also be a vector. DEFAULT: 0.3}
  \item{target}{target color that should be faded into. DEFAULT: "white"}
  \item{\dots}{Further arguments passed to \code{\link{colorRamp}}}
}
\value{character matirx with hexadecimal color codes.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Feb 2016}
\seealso{\code{\link{addAlpha}}, \code{\link{colorRamp}}, \code{\link{colors}}}
\examples{
plot(1:11, pch=16, cex=3, col=addFade(2, 10:0/10))
plot(1:11, pch=16, cex=3, col=addFade(2, 10:0/10, target="blue"))
plot(1:11, pch=16, cex=3, col=addFade(2, 10:0/10, target=3:4))
plot(1:21, pch=16, cex=3, col=addFade(2:3, 10:0/10))
plot(1:21, pch=16, cex=3, col=addFade(2:3, 10:0/10, target=4:5))
NewColors <- addFade(c("red","blue","yellow","green", "purple"), 0:200/200)
plot(runif(1000), col=NewColors, pch=16, cex=2)
}
\keyword{dplot}
\keyword{color}
