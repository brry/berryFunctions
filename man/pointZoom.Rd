\name{pointZoom}
\alias{pointZoom}
\title{zoom in originally static x11 graphics}
\description{zoom in x11 graphics - uses locator to define region to zoom into}
\usage{pointZoom(x, y=NA, z=NA, Time=1, steps=30, las=1, colp=FALSE,
xlab=substitute(x), ylab=substitute(y), notify=TRUE, ...)}
\arguments{
  \item{x}{same x coordinates as in current plot. x can be a matrix, then the y (and z) coordinates are taken from the second (and third) column.}
  \item{y}{ditto}
  \item{z}{if using colpoints, z-value }
  \item{Time}{Duration of zooming (speed) in seconds. DEFAULT: 1}
  \item{steps}{number of single zoomlevels. DEFAULT: 30}
  \item{las}{label axis style, see \code{\link{par}}. DEFAULT: 1}
  \item{colp}{logical: use \code{\link{colPoints}} when zooming? DEFAULT: FALSE}
  \item{xlab}{xlabel See \code{\link{plot}}. DEFAULT: substitute(x)}
  \item{ylab}{dito}
  \item{notify}{logical. Should notifications (instructions) be written to the console? DEFAULT: TRUE}
  \item{\dots}{further arguments passed to plot or colpoints.}
}
\value{none, works in existing graphics}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, ca 2012}
\note{Extensive testing is yet to be done!}

\seealso{\code{\link{shapeZoom}}, \code{\link{colPoints}}, \code{\link{locator}} }
\examples{
## Examples rely on locator, so can't be checked in non-interactive R use.
\dontrun{
a <- rnorm(90); b <- rexp(90)
windows(record=TRUE) # turn recording on
plot(a,b, las=1)
pointZoom(a,b)
# now scroll through the plots (Pg Up and Pg Dn) to unzoom again.

d <- data.frame(a,b)
class(d)
plot(d)
pointZoom(d)
}
}
\keyword{aplot}
\keyword{iplot}
\keyword{dynamic}
