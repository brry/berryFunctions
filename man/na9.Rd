\name{na9}
\alias{na9}
\title{prepend spaces before na.strings}
\description{Returns a number of useful character strings with varying amount of spaces prepended.
It can be used as \code{na.strings=na9()} in \code{\link{read.table}}.}
\usage{na9(nspace=5, base=c("-9999","-999", "-99", "-9.99", "-9.9", "-9,99", "-9,9"),
more="", ...)}
\arguments{
  \item{nspace}{number of spaces prepended. DEFAULT: 5}
  \item{base}{basic na.string structures. Might be expanded in the future. Thankful for suggestions. DEFAULT: c("-9999","-999", "-99", "-9.99", "-9.9", "-9,99", "-9,9")}
  \item{more}{allows more structures added to base. DEFAULT: ""}
  \item{\dots}{Arguments passed to nothing currently}
}
\value{Character strings}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Jan 2016}
\seealso{\code{\link{paste}} }
\examples{
na9()
}
\keyword{IO}
\keyword{file}
