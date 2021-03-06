% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/horizHist.R
\name{horizHist}
\alias{horizHist}
\title{Horizontal histogram}
\usage{
horizHist(
  Data,
  breaks = "Sturges",
  freq = TRUE,
  plot = TRUE,
  col = par("bg"),
  border = par("fg"),
  las = 1,
  xlab = if (freq) "Frequency" else "Density",
  main = paste("Histogram of", deparse(substitute(Data))),
  ylim = range(HBreaks),
  labelat = pretty(ylim),
  labels = labelat,
  ...
)
}
\arguments{
\item{Data}{any data that \code{\link{hist}} would take.}

\item{breaks}{character or numerical as explained in \code{\link{hist}}. DEFAULT: "Sturges"}

\item{freq}{logical. if TRUE, the histogram graphic is a representation of frequencies, the counts component of the result;
if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one). DEFAULT: TRUE}

\item{plot}{logical. Should histogram be plotted? FALSE to get just the hpos function. DEFAULT: TRUE}

\item{col}{color. DEFAULT: par("bg")}

\item{border}{color of borders of bars. DEFAULT: par("fg")}

\item{las}{integer. Label axis style. DEFAULT: 1}

\item{xlab}{character. Label for x-axis. DEFAULT: "absolute frequency"}

\item{main}{character. Title for graphic. DEFAULT: "Histogram of substitute(Data)"}

\item{ylim}{numerical vector of two elements. Y-axis limits. DEFAULT: range of data}

\item{labelat}{numerical vector. Position of Y-Axis labels. DEFAULT: pretty(ylim)}

\item{labels}{numerical or character. The labels themselves. DEFAULT: labelat}

\item{\dots}{further arguments passed to \code{\link{barplot}} and \code{\link{axis}}}
}
\value{
function to address y-coordinates
}
\description{
Draw a histogram with bars horizontally
}
\details{
Uses barplot to draw the histogram horizontally.
}
\note{
Doesn't work with breakpoints provided as a vector with different widths of the bars.\cr
     Please do not forget to use the function for vertical positioning from the \bold{current} horizontal histogram.
     If It is not working correctly, you might have the function defined from some prior horizHist result.
}
\examples{

# Data and basic concept
set.seed(8); ExampleData <- rnorm(50,8,5)+5
hist(ExampleData)
hpos <- horizHist(ExampleData)
# Caution: the labels at the y-axis are not the real coordinates!
# abline(h=2) will draw above the second bar, not at the label value 2.
# Use hpos (horizontal position), the function returned by horizHist:
abline(h=hpos(11), col=2, lwd=2)

# Further arguments
horizHist(ExampleData, xlim=c(-8,20))
horizHist(ExampleData, ylab="the ... argument worked!", col.axis=3)
hist(ExampleData, xlim=c(-10,40)) # with xlim
horizHist(ExampleData, ylim=c(-10,40), border="red") # with ylim
hpos <- horizHist(ExampleData, breaks=20, col="orange")
axis(2, hpos(0:10), labels=FALSE, col=2) # another use of hpos()

}
\seealso{
\code{\link{hist}}, \code{\link{barplot}}, \code{\link{axis}}
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de}, 2011-2012
}
\keyword{hplot}
