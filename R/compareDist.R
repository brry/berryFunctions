#' @title compare distributions
#' @description compare multiple distributions. All based on columns in a data.frame.
#'              Creates several plots based on the integers present in \code{plot}.
#' @return df, invisible
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2020
#' @seealso \code{\link{groupHist}}
#' @keywords hplot
#' @importFrom utils stack
#' @importFrom graphics plot polygon grid legend boxplot
#' @export
#' @examples
#' fakedata <- data.frame(norm=rnorm(30), exp=rexp(30), unif=runif(30))
#' compareDist(fakedata)
#'
#' @param df         Data.frame with (named) columns.
#' @param plot       Integers: which graphics to plot? 
#'                   Plot 1: overlaid density estimates\cr
#'                   Plot 2: multipanel histogram\cr
#'                   Plot 3: boxplot\cr
#'                   Plot 4: violin plot, if package \code{vioplot} is available.\cr
#'                   DEFAULT: 1:4
#' @param bw         Bandwidth passed to \code{\link{density}} for plot 1. DEFAULT: "SJ"
#' @param col        Color (vector). DEFAULT: \code{\link{catPal}(ncol(df), alpha=0.3)}
#' @param main       Title. DEFAULT: "Distributions of [df name]"
#' @param xlab,ylab  Axis labels for plot 1. DEFAULT: xlab="Values", ylab="Density"
#' @param legpos1,legpos2 Legend position for plot 1. DEFAULT: "topleft", NULL
#' @param horizontal Should boxplot and vioplot (plot 3 and 4) be horizontal? DEFAULT: FALSE
#' @param \dots      Further arguments passed to \code{\link{polygon}} (plot 1), 
#'                   \code{\link{groupHist}} (plot 2)
#'                   \code{\link{boxplot}} (plot 3) and 
#'                   \code{vioplot::\link[vioplot]{vioplot}} (plot 4)
#'
compareDist <- function(
df, 
plot=1:4,
bw="SJ", 
col=catPal(ncol(df), alpha=0.3), 
main=paste("Distributions of", deparse(substitute(df))), 
xlab="Values", 
ylab="Density",
legpos1="topleft",
legpos2=NULL,
horizontal=FALSE,
...)
{
force(main) # evaluate before accessing df
# overlaid density----
if(1 %in% plot){
dens <- lapply(df, density, bw=bw)
xlim <- range(sapply(dens, "[", "x"))
ylim <- range(sapply(dens, "[", "y"))
plot(1, type="n", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, main=main, panel.first=grid(), las=1)
for(i in  seq_along(dens)) polygon(dens[[i]], col=col[i], ...)
legend(legpos1, legpos2, colnames(df), fill=col, bty="n", border=NA)
}# multipanel hist----
if(2 %in% plot) groupHist(stack(df), x="values", g="ind", main=main, col=col, ...)
# boxplot----
if(3 %in% plot) boxplot(df, las=1, col=col, main=main, horizontal=horizontal, ...)
# violin plot----
if(4 %in% plot) if(requireNamespace("vioplot")) vioplot::vioplot(df, col=col, 
   las=1, main=main, horizontal=horizontal, ...)
return(invisible(df))
}
