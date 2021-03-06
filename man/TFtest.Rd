% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TFtest.R
\name{TFtest}
\alias{TFtest}
\title{Test logical expressions}
\usage{
TFtest(..., na = TRUE)
}
\arguments{
\item{\dots}{Expression(s) with logical operators to be evaluated,
with single letters for variables. Each expression is to be separated with a comma}

\item{na}{Logical: should NAs be included in the truth table? DEFAULT: TRUE}
}
\value{
Truth table as data.frame with TRUE and FALSE (and NA) combinations
}
\description{
Check if logical expressions return what you expect with a truth table
}
\details{
This is a nice way to check operator precedence, see \code{\link{Syntax}}
}
\examples{
TFtest(!a & !b)
TFtest(!a & !b, a&b, !(a&b))
TFtest(!a & !b | c)
TFtest(!a & !b | c, na=FALSE)
TFtest(!a)
TFtest(a&b|c, (a&b)|c, a&(b|c), na=FALSE) # AND has precedence over OR
TFtest(a|b, xor(a,b), na=FALSE)

}
\seealso{
\code{\link{logical}}
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de},  Mrz 2016
}
\keyword{logic}
