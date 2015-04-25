\name{linLogHist}
\alias{linLogHist}
\title{lin-log transition histogram}
\description{draw histograms that gradually transform from a linear to a logarithmic axis}
\usage{linLogHist(x, steps=100, breaks=20, col="blue", las=1,
xlab=deparse(substitute(x)), box=TRUE, axisargs=NULL, firstplot=TRUE, lastplot=TRUE,
write_t=TRUE, values_t=NULL, ...)}
\arguments{
  \item{x}{x values to be plotted in animation}
  \item{steps}{number of steps in transition. DEFAULT: 100}
  \item{breaks}{\code{\link{hist}} breaks. DEFAULT: 20}
  \item{col}{\code{\link{hist}} color. DEFAULT: "blue"}
  \item{las}{\code{\link{par}} LabelAxisStyle (numbers upright). DEFAULT: 1}
  \item{xlab}{Label for the x axis. DEFAULT: deparse(substitute(x))}
  \item{box}{Draw box at the end to overplot \code{\link{abline}s} crossing the box? DEFAULT: TRUE}
  \item{axisargs}{List of arguments passed to \code{\link{logVals}} and \code{\link{logAxis}}, like base. DEFAULT: NULL}
  \item{firstplot}{plot on linear scale first? DEFAULT: TRUE}
  \item{lastplot}{plot on logarithmic scale at the end? DEFAULT: TRUE}
  \item{write_t}{write transformation value in lower right corner? DEFAULT: TRUE}
  \item{values_t}{Supply vector with values for transformation (1/t). Overides steps. If you have a better algorithm than I do, please let me know! DEFAULT: NULL}
  \item{\dots}{further arguments passed to \code{\link{hist}}, like freq, main, xlim, ylab. Excluded: x, xaxt, possibly add}
}
\value{Returned invisibly: transformation values used. Plotted: \code{steps} number of images.}
\note{It's best to save the plots into a pdf or wrap it within\cr \code{png("Transition\%03d"); linLogHist(x); dev.off()}}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, April 2015}
\seealso{\code{\link{linLogTrans}} }
\examples{
x <- rlnorm(700, m=3)
hist(x, col=4)
hist(log10(x), xaxt="n"); logAxis(1); hist(log10(x), col=4, add=TRUE)

\dontrun{
## Rcmd check --as-cran doesn't like to open external devices such as pdf,
## so this example is excluded from running in the checks.
pdf("LinLogTransitionAnimation.pdf")
linLogHist(x, main="Example Transition", steps=20, freq=FALSE)
dev.off()

# if you have FFmpeg installed, you can use the animation package like this:
library2(animation)
saveVideo(linLogHist(x, steps=50), video.name="linlog_anim.mp4", interval=0.08,
ffmpeg="C:/ffmpeg-20150424-git-cd69c0e-win64-static/bin/ffmpeg.exe")
}
}
\keyword{dplot}
\keyword{hplot}
\keyword{dynamic}
