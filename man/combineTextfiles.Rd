\name{combineTextfiles}
\alias{combineTextfiles}
\title{combine several Textfiles into one}
\description{Combine several textfiles into one, regardless of their content.}
\usage{combineTextfiles(inFiles=dir(), inDir=getwd(),
outFile="combined_Textfiles.txt", outDir=inDir, sep=NULL, names=TRUE)}
\arguments{
  \item{inFiles}{vector with names of input files, as can be read with \code{\link{scan}}. Is pasted with inDir, so don't use full paths. DEFAULT: dir()}
  \item{inDir}{Character string: path to the files. E.g. "D:/MyFolder/Subfolder". Don't have / at the end. DEFAULT: \code{\link{getwd}}().}
  \item{outFile}{Character string: name of the file to be created. Again, just the file name, not a path. DEFAULT: "combined_Textfiles.txt"}
  \item{outDir}{Character string: path for output file. DEFAULT: inDir}
  \item{sep}{Character string: Separation between content of each file and the following. DEFAULT: NULL, with which it uses an empty line, two lines with dashes, and another line break.}
  \item{names}{Should File names be included after sep? DEFAULT: TRUE}
}
\value{None, but prints number of files combined and output file name.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Nov 2012}
\seealso{\code{\link{paste}}, \code{\link{scan}}, \code{\link{write}}, \code{\link{cat}}.}
\examples{
# These are skipped by rcmd check (writing to external places is not allowed)
\dontrun{
cat("This is Sparta.\\nKicking your face.", file="BujakashaBerry1.txt")
cat("Chuck Norris will roundhousekick you.", file="BujakashaBerry2.txt")
combineTextfiles(inFiles=paste0("BujakashaBerry", 1:2, ".txt"), outFile="Ausgabe.txt")
file.show("Ausgabe.txt")
unlink(c(paste0("BujakashaBerry", 1:2, ".txt"),"Ausgabe.txt"))
}
}
\keyword{IO}
\keyword{file}
\keyword{character}
