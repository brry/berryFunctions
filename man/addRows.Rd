\name{addRows}
\alias{addRows}
\title{add n rows to a data.frame}
\description{simple Helper-Function to add n rows to a data.frame.}
\usage{addRows(df, n, values = NA)}
\arguments{
  \item{df}{Dataframe object}
  \item{n}{Number of rows to add}
  \item{values}{Values to be used in the new rows. DEFAULT: NA}
}
\value{A data.frame}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2014}
\seealso{ \code{\link{data.frame}}, \code{\link{matrix}}, \code{\link{rbind}}}
\examples{
MYDF <- data.frame(A=5:3, B=2:4)
addRows(MYDF, 3)
}
\keyword{ misc }

