\name{dataStr}
\alias{dataStr}
\title{str of datasets}
\description{Print the \code{\link{str}} of each dataset returned by \code{\link{data}, by default in the package \code{\link{datasets}}}
\usage{dataStr(package="datasets", ... )}
\arguments{
  \item{package}{package. DEFAULT: "datasets"}
  \item{\dots}{other arguments passed to \code{\link{data}}}
}
\value{NULL. prints via \code{\link{message}} in a for loop.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, November 2015}
\seealso{\code{\link{str}} }
\examples{
dataStr()
}
\keyword{print}
\keyword{documentation}
