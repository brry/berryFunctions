\name{sortDF}
\alias{sortDF}
\title{sort dataframes by column}
\description{sort a data.frame by column - basically just a wrapper for order}
\usage{sortDF(df, col, ... )}
\arguments{
  \item{df}{Data.frame to be sorted}
  \item{col}{Column (index or name) to be sorted by}
  \item{\dots}{Further arguments passed to \code{\link{order}}, like eg \code{decreasing=TRUE}}
}
\value{data.frame}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, June 2015}
\seealso{\code{\link{sort}}, \code{\link{order}}}
\examples{
sortDF(USArrests[USArrests$Murder>8,], "Assault")
sortDF(USArrests[USArrests$Murder>8,], 3, decreasing=TRUE)
}
\keyword{univar}
\keyword{manip}
\keyword{arith}
