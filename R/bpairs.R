#' @title customized pairs plot
#' @description pairs plot with cor in the lower panel (can handle NAs), 
#' nice hist on the diagonal, nice scatterplot in the upper panel.
#' Based on the examples in pairs.
#' @return invisible NULL
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb 2025
#' @seealso \code{graphics::\link[graphics]{pairs}}
#' @keywords aplot
#' @importFrom graphics pairs
#' @export
#' @examples
#' bpairs(mtcars[,1:5])
#' mtcarsNAs <- mtcars[,1:5]
#' mtcarsNAs[2,3] <- NA
#' mtcarsNAs[2:8,4] <- NA
#' bpairs(mtcarsNAs)
#' bpairs(iris)
#'
#' @param df    Data.frame. Can contain NAs. Character columns are excluded.
#' @param main  Title for the overall graph. DEFAULT: NULL (from input)
#' @param pch   Point character. DEFAULT: 16
#' @param col   Color. DEFAULT: \code{\link{addAlpha}("blue")}
#' @param \dots Further arguments passed to \code{\link{pairs}}
#'
bpairs <- function(
df,
main=NULL,
pch=16,
col=addAlpha("blue"),
...
)
{
if(is.null(main)) main <- deparse(substitute(df))
df <- df[!sapply(df, is.character)]
panel.hist <- function(x, ...)
  {
  usr <- par("usr")
  par(usr=c(usr[1:2], 0, 1.5) )
  h <- hist(x, breaks=15, plot=FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="salmon")
  nNA <- sum(is.na(x))
  if(nNA>0) textField(mean(usr[1:2]), 0.2, paste(nNA, "NA"), fill=addAlpha("white",0.7), cex=1)
  axis(1)
  box()
  }
panel <- function(x, y, pch=pch, col=col, ....)
  {
  points(x, y, pch=pch, col=col, ...)
  r <- abs(cor(x, y, use="complete.obs"))
  usr <- par("usr")
  text(mean(usr[1:2]), mean(usr[3:4]), round0(r,2,0), cex=exp(r))
  }
pairs(df, panel=panel, diag.panel=panel.hist, lower.panel=NULL, pch=pch, col=col, ...)
title(main=main, outer=TRUE, line=-1)
invisible(NULL)
}
