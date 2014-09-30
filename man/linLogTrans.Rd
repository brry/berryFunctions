\name{linLogTrans}
\alias{linLogTrans}
\title{Animation for transition from linear to logarithmic axis}
\description{make images that gradually transform from a linear to a logarithmic axis}
\usage{linLogTrans(x, y, log="x", steps=100, base=1, firstplot=TRUE, lastplot=TRUE, 
write_t=TRUE, values_t=NULL, pointsarg=NULL, ...) }
\arguments{
  \item{x}{x values to be plotted in animation}
  \item{y}{vector with corresponding y values}
  \item{log}{which axis is logarithmic, "x" or "y". DEFAULT: "x"}
  \item{steps}{Approximate number of steps (images) in transition (About 30\% are taken out). DEFAULT: 100}
  \item{base}{base passed to \code{\link{logVals}}. DEFAULT: 1}
  \item{firstplot}{Plot data on linear axis as first image? DEFAULT: TRUE}
  \item{lastplot}{Plot data on logarithmic axis as last image? DEFAULT: TRUE}
  \item{write_t}{write transformation value in lower right corner? DEFAULT: TRUE}
  \item{values_t}{Supply vector with values for transformation (1/t). Overides steps. If you have a better algorithm than I do, please let me know! DEFAULT: NULL for internal calculation based on size of steps.}
  \item{pointsarg}{List of further arguments passed to points, like pch, cex, col. DEFAULT: NULL}
  \item{\dots}{further arguments passed only to plot, like main, xlim, ylab. Excluded: x, y, las, xaxt, type}
}
\value{returned invisibly: transformation values used. Plotted: steps number of images.}
\note{It's best to save the plots into a pdf (see the example) or wrap it within\cr \code{png("Transition\%03d"); linLogTrans(x,y); dev.off()}}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, June 2014}
\references{x^(1/t) is based on the first comment on \url{http://stackoverflow.com/questions/15994442/}\cr
besides the nice graphic properties, check this page for the implications on rates of change: \cr
\url{http://sfew.websitetoolbox.com/post/show_single_post?pid=1282690259&postcount=4}\cr
\url{http://sfew.websitetoolbox.com/post/show_single_post?pid=1282691799&postcount=5}}
\seealso{\code{\link{logVals}} }
\examples{

set.seed(42);  x <- 10^rnorm(100, 3);  y <- runif(100)
linLogTrans(x,y, steps=15)


\dontrun{
## Rcmd check --as-cran doesn't like to open external devices such as pdf,
## so this example is excluded from running in the checks.
pdf("LinLogTransitionAnimation.pdf")
linLogTrans(x,y, main="Example Transition")
dev.off()

}
}
\keyword{dplot}
\keyword{hplot}
\keyword{dynamic}
