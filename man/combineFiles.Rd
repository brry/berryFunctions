\name{combineFiles}
\alias{combineFiles}
\title{combine several Textfiles into one}
\description{Combine several textfiles into one, regardless of their content.}
\usage{combineFiles(inFiles=dir(), inDir=getwd(),
outFile="combined_Textfiles.txt", outDir=inDir,
sep=NULL, names=TRUE, selection=NULL, quiet=FALSE)}
\arguments{
  \item{inFiles}{vector with names of input files, as can be read with \code{\link{scan}}. Is pasted with inDir, so don't use full paths. DEFAULT: dir()}
  \item{inDir}{Character string: path to the files. E.g. "D:/MyFolder/Subfolder". Don't have / at the end. DEFAULT: \code{\link{getwd}}().}
  \item{outFile}{Character string: name of the file to be created. Again, just the file name, not a path. DEFAULT: "combined_Textfiles.txt"}
  \item{outDir}{Character string: path for output file. DEFAULT: inDir}
  \item{sep}{Character string: Separation between content of each file and the following. DEFAULT: NULL, with which it uses an empty line, two lines with dashes, and another line break.}
  \item{names}{Should File names be included after sep? DEFAULT: TRUE}
  \item{selection}{Index of rows that should be written. Can refer to each file separately, e.g. \code{substr(inFile_i,1,1)=="#"}, DEFAULT: all lines}
  \item{quiet}{Suppress message about number of files combined? DEFAULT: FALSE}
}
\value{None, but prints number of files combined and output file name.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Nov 2012}
\seealso{\code{\link{compareFiles}}, and the functions used internally here, namely: \code{\link{paste}}, \code{\link{scan}}, \code{\link{write}}.}
\examples{
# These are skipped by rcmd check (writing to external places is not allowed)
\dontrun{
cat("This is Sparta.\\nKicking your face.", file="BujakashaBerry1.txt")
cat("Chuck Norris will roundhousekick you.", file="BujakashaBerry2.txt")
combineFiles(inFiles=paste0("BujakashaBerry", 1:2, ".txt"), 
             outFile="BujakashaBerry3.txt")
file.show("BujakashaBerry3.txt")
unlink(paste0("BujakashaBerry", 1:3, ".txt"))
}
}
\keyword{IO}
\keyword{file}
\keyword{character}
