\name{removeSpace}
\alias{removeSpace}
\title{Remove white spaces from strings}
\description{Remove leading and or trailing white space from character strings}
\usage{removeSpace(x, begin=TRUE, end=TRUE, all=FALSE, ... )}
\arguments{
  \item{x}{Character string, can be a vector}
  \item{begin}{Logical. Remove leading spaces at the beginning of the character string? DEFAULT: TRUE}
  \item{end}{Logical. Remove trailing spaces at the end? DEFAULT: TRUE}
  \item{all}{Logical. Remove all spaces anywhere in the string? DEFAULT: FALSE}
  \item{\dots}{Further arguments passed to \code{\link{sub}} or \code{\link{gsub}}, like \code{ignore.case, perl, fixed, useBytes}.}
}
\value{Character string (vector)}
\note{If all arguments are FALSE, the string is returned unchanged.\cr
Not extensively tested yet, please mail me any problems...}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Dec 2014}
\seealso{\code{\link{sub}}}
\examples{
s <- c("space at end     ", "  white at begin", "  both ", " special ^  ")
removeSpace(s)
}
\keyword{character}
