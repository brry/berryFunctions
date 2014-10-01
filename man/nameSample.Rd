\name{nameSample}
\alias{nameSample}
\title{Nonrandom character sequence with sample}
\description{Find the seed necessary to produce a character sequence by using sample
Can especially impress newbies a lot.}
\usage{nameSample(name)}
\arguments{
  \item{name}{Character string. long strings (>>5) will compute a VERY long time!}
}
\value{\code{\link{cat}}s command into the console that can be copypasted to anyone's R script.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, April 2014}
\note{nameSample may take a lot of time, due to nchar^26 possibilities.
That's why it warns about strings longer than 5 characters}
\seealso{ \code{\link{yearSample}} to wish a happy new year, \code{\link{set.seed}}, \code{\link{sample}}, \code{\link{letters}}    }
\examples{
\dontrun{
nameSample("berry")
}
# After that, you can send the result to colleagues:
# Kind regards from
set.seed(1248272)
paste(sample(letters, 5, replace=TRUE), collapse='')
}
\keyword{character}

