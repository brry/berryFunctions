#' Table with values with value-dependent colored backgrounds in pdf
#' 
#' Table with numbers and corresponding color in the background of each cell.
#' (heatmap)
#' 
#' @details Create tables with corresponding color in the background of each cell. (heatmap)
#' 
#' @return List of locations in plot.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2012 + Nov 2016
#' @seealso \code{\link{pdf}}, \code{\link{heatmap}}
#' @keywords hplot
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics abline par plot rect text
#' @export
#' @examples
#' 
#' Bsp <- matrix(c(21,23,26,27, 18,24,25,28, 14,17,23,23, 16,19,21,25), ncol=4, byrow=TRUE)
#' colnames(Bsp) <- paste0("Measure", LETTERS[1:4])
#' rownames(Bsp) <- paste("prod", 8:11, sep="_")
#' Bsp
#' 
#' (  tableColVal(Bsp)   )
#' tableColVal(Bsp, nameswidth=0.1) # relative to plot width
#' tableColVal(Bsp, namesheight=0.5, srt=45)
#' tableColVal(Bsp, namesheight=0.5, colargs=c(srt=45))
#' 
#' tableColVal(Bsp, cellargs=list(cex=2), col="red")
#' tableColVal(Bsp, Range=c(10,40))
#' tableColVal(Bsp, Range=c(20,40))
#' tableColVal(Bsp, palette=heat.colors(12))
#' tableColVal(Bsp, palette=c(2,4,7), main="more\nstuff")
#' 
#' Bsp2 <- matrix(rexp(30), ncol=6, byrow=TRUE)
#' (  tableColVal(Bsp2)  )
#' tableColVal(Bsp2, digits=4)
#' colPointsLegend(Bsp2, horizontal=FALSE, x1=0.05, x2=0.15, y1=0.1, y2=0.8, title="")
#' 
#' \dontrun{
#' ## Rcmd check --as-cran doesn't like to open external devices such as pdf,
#' ## so this example is excluded from running in the checks.
#' pdf("TableColVal.pdf", height=5);  tableColVal(Bsp);  dev.off()
#' openFile("TableColVal.pdf")
#' unlink("TableColVal.pdf")
#' }
#' 
#' @param mat         Matrix with values and row/column names
#' @param main        Title for topleft space. DEFAULT: name of mat object.
#' @param nameswidth  Relative width of row names at the left, as a percentage of plot.
#'                    DEFAULT: 0.3
#' @param namesheight Relative height of column names at the top. DEFAULT: 0.1
#' @param palette     Color palette for the heatmap. DEFAULT: \code{\link{seqPal}(100)}
#' @param Range       Range mapped to color palette. DEFAULT: range(mat)
#' @param digits      Number of digits rounded to for writing. DEFAULT: 2
#' @param classargs   List of arguments specifying how to call \code{\link{classify}},
#'                    e.g. method. DEFAULT: NULL
#' @param cellargs,colargs,rowargs,mainargs List of arguments passed to \code{\link{text}} 
#'                    only for the cells, column labels, row labels or title,
#'                    respectively. DEFAULTS: NULL
#' @param \dots Further arguments passed to all \code{\link{text}} like cex, col, srt, ...
#' 
tableColVal <- function(
mat,
main=deparse(substitute(mat)),
nameswidth=0.3,
namesheight=0.1,
palette=seqPal(100),
Range=range(mat,finite=TRUE),
digits=2,
...,
classargs=NULL,
cellargs=NULL,
colargs=NULL,
rowargs=NULL,
mainargs=NULL
)
{
main <- main # evaluate promise before mat is evaluated
mat <- as.matrix(mat)
nc <- ncol(mat)
nr <- nrow(mat)
# set plot
op <- par(mar=rep(0,4))
on.exit(par(op))
plot(1, type="n", xlim=0:1, ylim=0:1, xaxs="i", yaxs="i", axes=FALSE, ann=FALSE)
# set positions for text and lines
x2 <- seq(from=nameswidth,  to=1, len=nc+1) # right ends of cells
x1 <- c(0, x2[1:nc] )                       # left
xm <- colMeans(rbind(x1,x2))                # horizontal centers
y2 <- seq(from=1-namesheight, to=0, len=nr+1) # top ends of cells
y1 <- c(1, y2[1:nr])                          # bottom
ym <- colMeans(rbind(y1,y2))                  # vertical centers
# define color for each value of mat
cl <- do.call(classify, args=owa(list(x=mat, breaks=length(palette), Range=Range), classargs))
# plot rectancles with colors corresponding to values of mat
rect(xleft=rep(x1[-1], each=nr), xright=rep(x2[-1], each=nr), border=NA,
      ytop=rep(y1[-1], nc),     ybottom=rep(y2[-1], nc), col=palette[cl$index])
abline(v=c(x1,1), h=c(y1,1))
# add text to each cell:
def <- list(x=rep(xm[-1], each=nr), y=rep(ym[-1], nc), labels=round0(mat, digits, pre=1))
def2 <- list(...)
do.call(text, args=owa(c(def,def2), cellargs))
# add "titles"
do.call(text, args=owa(c(list(x=xm[-1], y=ym[1] , labels=colnames(mat)),def2), colargs))
do.call(text, args=owa(c(list(x=xm[1] , y=ym[1] , labels=main),         def2), mainargs))
do.call(text, args=owa(c(list(x=xm[1] , y=ym[-1], labels=rownames(mat)),def2), rowargs))
# output
return(invisible(list(x1=x1,x2=x2,xm=xm,y1=y1,y2=y2,ym=ym)))
} # end of function

