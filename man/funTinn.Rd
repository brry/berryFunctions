\name{funTinn}
\alias{funTinn}
\title{open function in TinnR}
\description{Opens function or object in external editor with an R command}
\usage{funTinn(name) }
\arguments{
  \item{name}{name of function or object to be opened with the program associated with .r-files. In my case, the editor Tinn-R}
  }
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2014}
\seealso{\code{\link{edit}} }
\examples{
\dontrun{
funTinn(boxplot.default)
}
}
\keyword{connection}
