# plot Quantile Bands
# Berry Boessenkool, Sept 2014

quantileBands <- function(
mat, # Matrix or data.frame with columns of data
x=1:ncol(mat), # X-axis positions for each column
col=rgb(0,0,1, alpha=c(0.5, 0.7)), # Vector of colors for each quantile group, recycled reversively if necessary
add=FALSE, # Add to existing plot? Allows to add to highly customized plot
main="Quantile Bands", ylab="", xlab="", # plot labels
probs=0:4/4, # Probabilities passed to \code{\link{quantile}}
na.rm=FALSE, # Remove NAs before computing \code{\link{quantile}s}, \code{\link{median}} and \code{\link{mean}}?
type=7, # Which of the 9 \code{\link{quantile}} algorithms should be used
smooth=NA, # If(!is.na), \code{width} passed to \code{\link{movAv}} smoothing quantiles
medargs=NULL, # List of arguments passed to lines drawing \code{\link{median}}. Not drawn if NULL
meanargs=NULL, # List of arguments passed to lines drawing \code{\link{mean}}. Not drawn if NULL
txi, # text x position index (along columns of mat), recyled if necessary.
textargs=NULL, # List of arguments passed to \code{\link{text}}, like col, adj, ...
...) # Further arguments passed to \code{\link{polygon}}, like border, lty, ...
{
# input check:------------------------------------------------------------------
if(length(x) != ncol(mat)) stop("length(x) (",length(x),") must equal ncol(mat) (",ncol(mat),").")
if(!is.vector(x)) stop("x must be a vector.")
if(!is.numeric(x)) stop("x must be numeric.")
# number of classes (and thus, polygon calls)
nclass <- length(probs)-1
# recycle color vectors (with reverse):
Ncol <- length(col)
if(Ncol > nclass) col <- col[1:nclass]
if(Ncol==1)
  col2 <- rep(col, nclass)
else
if(Ncol == nclass)
  col2 <- col
else
if(Ncol <= ceiling(nclass/2))
  {
  col2 <- rep(NA, nclass)
  col2[1:Ncol] <- col
  col2[nclass:(nclass-Ncol+1)] <- col # this is reverse
  col2[is.na(col2)] <- tail(col,1)
  }
else stop("col has wrong length (",Ncol,"). mus either be ==1, ==length(probs)-1 (==",
         nclass,") or <=length(probs)/2 (==", ceiling(nclass/2),").")
#
# Quantiles for each column
qc <- apply(mat, MARGIN=2, quantile, probs, na.rm, type)
# smooth:
if(!is.na(smooth))
  {
  qc <- t(apply(qc, MARGIN=1, movAv, smooth))
  nna <- !is.na(qc[1,])
  qc <- qc[,nna]
  x <- x[nna]
  }
else nna <- 1:ncol(qc)
# plot initiation:
if(!add) plot(x, qc[1,], type="n", ylim=range(qc, na.rm=TRUE),
              main=main, ylab=ylab, xlab=xlab)
# actual polygon drawing:
for(i in 1:nclass)
   polygon(x=c(x, rev(x)), y=c(qc[i,], rev(qc[i+1,])), col=col2[i], ...)
# add median line:
if(!is.null(medargs))
   {
   medians <- apply(mat, MARGIN=2, median, na.rm=na.rm)[nna]
   do.call(lines, owa(list(x=x, y=medians), medargs))
   }
# add mean line:
if(!is.null(meanargs))
   {
   means <- apply(mat, MARGIN=2, mean, na.rm=na.rm)[nna]
   do.call(lines, owa(list(x=x, y=means), meanargs))
   }
# text
if(missing(txi)) txi <- round(ncol(qc)/2)
if(!is.na(txi))
  {
  txi <- rep(txi, length=nrow(qc))
  utxi <- unique(txi)
  if(any(utxi < 1)) stop("txi (",utxi,") must be a vactor of positive integers.")
  if(any(utxi > ncol(qc))) stop("txi (",utxi,") must be smaller than ncol(mat)-smooth (",ncol(qc),").")
  do.call(text, args=owa(list(x=x[txi], y=diag(qc[,txi]), labels=rownames(qc)), textargs))
  }
# output
colnames(qc) <- x
return(invisible(qc))
} # Function end
