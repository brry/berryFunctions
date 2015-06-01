\name{require2}
\alias{require2}
\alias{library2}
\title{install.package and require}
\description{If a package is not available, it is installed before being loaded}
\usage{require2(name, ...); library2(name, ...)}
\arguments{
  \item{name}{Name of the package(s). Can be qouted, must not.}
  \item{\dots}{Arguments passed to \code{\link{install.packages}} like \code{lib}, \code{repos} etc.}
}
\value{Cats help instruction.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2014}
\note{Passing a vector with packages will work, but give some warnings.}
\seealso{\code{\link{install.packages}}, \code{\link{require}}}
\examples{
\dontrun{
## Excluded fom CRAN checks. Package installation on server is unnecessary.
require2(ada)
}
}
\keyword{package}
