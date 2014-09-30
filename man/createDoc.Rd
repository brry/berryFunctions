\name{createDoc}
\alias{createDoc}
\title{Create Documentation file from .r-source}
\description{Create Documentation file (especially section arguments) from .r-source}
\usage{createDoc(fun, path = "S:/Dropbox/Public/berryFunctions") }
\arguments{
  \item{fun}{Character string or unqouted name. Function (== filename) with correct structure in source code.}
  \item{path}{Path to package in development containing folders 'R' and 'man'. DEFAULT: "S:/Dropbox/Public/berryFunctions"}
}
\details{This assumes the following structure of source code:\cr
          \code{MyFun <- function(}\cr
          \code{arg1, # Explanation of this item}\cr
          \code{arg2=TRUE, # Ditto, with default}\cr
          \code{arg3)}\cr
          \code{\{}
          \code{do this and that. The above line may ONLY contain the curly brace}
}
\value{None. Cats documentation for \code{fun} in \code{path}/man. Only usage, arguments and author section are filled, the rest is empty (but the frame is there).}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, June 2014}
\section{Warning}{This is highly specific to my way of working, don't rely blindly on it.}
\seealso{\code{\link{package.skeleton}}, \code{\link{prompt}}, \code{\link{scan}}, \code{\link{cat}}, Roxygen and Roxygen2 \url{http://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html}}
\examples{
#createDoc("textField")
}
\keyword{documentation}
