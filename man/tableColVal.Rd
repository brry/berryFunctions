\name{tableColVal}
\alias{tableColVal}
\title{Table with values with value-dependent colored backgrounds in pdf}
\description{Table with numbers and corresponding color in the background of each cell. (heatmap)}
\usage{tableColVal(mat, pdffile="table_col_val.pdf", pdf=FALSE, nameswidth=0.3,
namesheight=0.1, palette=rainbow(nrow(mat)*ncol(mat), start=0, end=0.7),
Range=range(mat,finite=TRUE), 
argrow=NULL, argcol=NULL, argcell=NULL, argmain=NULL, ...)}
\arguments{
  \item{mat}{Matrix with values}
  \item{pdffile}{Name of File to write to. DEFAULT: "table_col_val.pdf"}
  \item{pdf}{Should table be written to pdffile? (Else it will plot in x11). DEFAULT: FALSE}
  \item{nameswidth}{Relative width of row names at the left, as a percentage of plot. DEFAULT: 0.3}
  \item{namesheight}{Relative height of column names at the top. DEFAULT: 0.1}
  \item{palette}{Color palette for the heatmap. DEFAULT: rainbow(nrow(mat)*ncol(mat), start=0, end=0.7)}
  \item{Range}{Range of values mapped linearly to color palette. DEFAULT: range(mat,finite}
  \item{argrow, argcol, argcell, argmain}{List of arguments passed to \code{\link{text}} in row and column names, cell content and topleft cell, respectively. Could be cex, col, srt, etc. DEFAULTS: NULL} 
  \item{\dots}{Further arguments passed to \code{\link{pdf}}.}
}
\value{None. PDF or plot produced.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Nov. 2012}
\seealso{\code{\link{pdf}}, \code{\link{heatmap}}}
\examples{
Bsp <- matrix(c(21,23,26,27, 18,24,25,28, 14,17,23,23, 16,19,21,25), ncol=4, byrow=TRUE)
colnames(Bsp) <- paste0("Measure", LETTERS[1:4])
rownames(Bsp) <- paste("prod", 8:11, sep="_")
Bsp

tableColVal(Bsp)
tableColVal(Bsp, nameswidth=0.1) # relative to plot width
tableColVal(Bsp, namesheight=0.5, argcol=list(srt=90))

tableColVal(Bsp, argrow=list(col="red", cex=2) )
tableColVal(Bsp, Range=c(10,40))
tableColVal(Bsp, palette=heat.colors(12))
tableColVal(Bsp, palette=c(2,4,7), argmain=list(labels="last\ncomparison"))

\dontrun{
tableColVal(Bsp, pdf=TRUE, width=12)  # further arguments to pdf possible.

Bsp2 <- matrix(sample(1:100, 30), ncol=6, byrow=TRUE)
graphics.off(); X11(height=4)
tableColVal(Bsp2)
}
}
\keyword{hplot}
