\name{lsMem}
\alias{lsMem}
\title{Show memory size of objects in MB}
\description{Show memory size of the biggest objects in MB}
\usage{lsMem(n = 6, pos = 1, ...)}
\arguments{
  \item{n}{Number of Objects to be shown separately. The rest is combined into "sum rest". DEFAULT: 6}
  \item{pos}{Environment where \code{\link{ls}} looks for objects.}
  \item{\dots}{Further arguments passed to \code{\link{ls}}}
}
\details{Helps you find the biggest memory killers.}
\value{Named vector with object sizes in MB (MegaBytes)}
\references{\url{http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session}}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Feb 2014}
\seealso{\code{\link{object.size}}, \code{\link{ls}}}
\examples{
\dontrun{
lsMem()
}
}
\keyword{ programming }
\keyword{ file }

