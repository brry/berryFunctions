\name{nameSample}
\alias{nameSample}
\title{Nonrandom character sequence with sample}
\description{Find the seed necessary to produce a character sequence by using sample
Can especially impress newbies a lot.}
\usage{nameSample(name, progress=nc>3)}
\arguments{
  \item{name}{Character string. long strings (>>5) will compute a VERY long time!}
  \item{progress}{Logical. Monitor progress by printing a dot every 10000 tries? DEFAULT: TRUE for long names (nchar(name)>3).}
}
\value{\code{\link{cat}}s command into the console that can be copypasted to anyone's R script.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, April 2014}
\note{nameSample may take a lot of time, due to nchar^26 possibilities.
That's why it warns about strings longer than 5 characters}
\seealso{ \code{\link{yearSample}} to wish a happy new year, \code{\link{set.seed}}, \code{\link{sample}}, \code{\link{letters}}    }
\examples{
# Not run in RCMD check as they're very time consuming
\dontrun{
nameSample("berry")  # After that, you can send the result to colleagues:
# Kind regards from
set.seed(1248272); paste(sample(letters,5,TRUE), collapse='')

# calculation time
                                       # on my slow laptop:   # on PC
system.time(nameSample("berr"))        # 25 s # berry: 57 s     10  23
system.time(nameSample("berr", FALSE)) # 23 s          53 s      9  20

# let <- sapply(1:4, function(n) apply(replicate(n, letters[sample(15)]), 1, paste, collapse=""))
# calctime <- sapply(let, function(x) system.time(nameSample(x, progress=F))[3])
# write.table(calctime, "calctime_nameSample.txt")
ctfile  <- system.file("extdata/calctime_nameSample.txt",  package="berryFunctions")
ctfile2 <- system.file("extdata/calctime_nameSample2.txt", package="berryFunctions")
calctime <- read.table(ctfile)
# regression result in hours:
expReg(nchar(rownames(calctime))-8, calctime[,1], xlim=c(1,7), ylim=c(-3,4),
       predict=7)/3600

# For my 3 times faster computer:
calctime <- read.table(ctfile2)
expReg(nchar(rownames(calctime))-8, calctime[,1], xlim=c(1,7), ylim=c(-3,4),
       predict=c(4,7))/c(1,3600)
# 4 sec for 4 letters are expected to be 10 hours for 7 letters...

}
}
\keyword{character}

