\name{timer}
\alias{timer}
\title{timer alarm}
\description{beeps in a given interval and gives a progress bar in the console}
\usage{timer(interval=20, n=15, write=FALSE )}
\arguments{
  \item{interval}{\code{\link{alarm}} interval in seconds. DEFAULT: 20}
  \item{n}{number of alarm signals to be given. DEFAULT: 15}
  \item{write}{Should the actual estimated time be written for overhead computing time control purposes? DEFAULT: FALSE}
}
\details{defaults to practice useR lightning talks: 15 slides, each shown 20 secs, change automatically}
\value{none}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, June 2015}
\references{\url{http://user2015.math.aau.dk/lightning_talks}}
\seealso{\code{\link{alarm}}, \code{\link{Sys.sleep}}, \code{\link{txtProgressBar}}}
\examples{
timer(interval=0.5, n=3)
timer(interval=0.2, n=8, write=TRUE) # a slight deviation occurs for a large n
# timer() # to practice lightning talks at useR! conferences
}
\keyword{utilities}
\keyword{chron}
