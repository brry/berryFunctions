\name{pastec}
\alias{pastec}
\title{Paste with collapse = ", "}
\description{\code{\link{paste}} with collapse = ", "}
\usage{pastec(..., sep=" ", collapse=", " )}
\arguments{
  \item{\dots}{Object(s) to be \code{\link{paste}d} to a character vector}
  \item{sep}{Character string to separate single strings. DEFAULT: " "}
  \item{collapse}{Character string between combined strings. DEFAULT: ", "}
}
\value{Single character string}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, April 2015}
\seealso{\code{\link{paste}}, \code{\link{paste0}} }
\examples{
listoferrors <- c("filetype", "header", "nonemptyline")
message("The following entities were corrupted:\n", pastec(listoferrors))
}
\keyword{character}
