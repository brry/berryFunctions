\name{nse}
\alias{nse}
\title{Nash-Sutcliffe efficiency}
\description{compute Nash-Sutcliffe efficiency from two vectors. Based on eval.NSeff  in RHydro Package}
\usage{nse(obs,sim)}
\arguments{
  \item{obs}{Vector with observational data.}
  \item{sim}{simulated data.}
}
\details{Basically does the following, after some input checks:
1 - ( sum((obs - sim)^2) / sum((obs - mean(obs))^2) )}
\value{Numeric.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, July 2013}
\note{NAs are omitted with warning.}
\seealso{\code{\link{rmse}}, \code{\link{rsquare}}}
\examples{
SIM <- dbeta(1:40/40, 3, 10)
OBS <- SIM + rnorm(20,,0.2)
plot(OBS)
lines(SIM)
nse(OBS, SIM)
}
\keyword{ts}
