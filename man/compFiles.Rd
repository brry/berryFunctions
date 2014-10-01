\name{compFiles}
\alias{compFiles}
\title{compare two textfiles for equality}
\description{returns the line numbers where two (text)files differ}
\usage{compFiles(file1, file2, nr=20, startline=1, endline=length(f1), quiet=FALSE, ... )}
\arguments{
  \item{file1, file2}{Filenames to be read by \code{\link{readLines}}.}
  \item{nr}{number of results printed. DEFAULT: 20}
  \item{startline, endline}{start and end lines, e.g. to exclude section that is already compared.}
  \item{quiet}{show warnings about file lengths? DEFAULT: FALSE}
  \item{\dots}{further arguments passed to \code{\link{readLines}}}
}
\value{result from \code{\link{head}}}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Aug 2014}
\seealso{\code{\link{combineTextfiles}} }
\examples{
filenames <- system.file(paste0("extdata/Versuch",1:2,".txt"), package="berryFunctions")
compFiles(filenames[1], filenames[2], warn=FALSE)
}
\keyword{IO}
\keyword{file}
\keyword{character}
