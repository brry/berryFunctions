\name{yearSample}
\alias{yearSample}
\title{Nonrandom year with sample}
\description{Nerdy way to wish someone a happy new year by using sample.
Can especially impress newbies a lot.}
\usage{yearSample(year)}
\arguments{
  \item{year}{4 digit numerical year.}
}
\value{\code{\link{cat}}s command into the console that can be copypasted to anyone's R script.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, April 2014}
\seealso{ \code{\link{nameSample}} to impress with "randomly" finding a name, \code{\link{set.seed}}, \code{\link{sample}}, \code{\link{letters}}    }
\examples{
yearSample(2015)
# Have a nerdy
set.seed(1244); sample(0:9, 4, replace=TRUE)
}

