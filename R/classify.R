# Berry Boessenkool, Aug/Sept 2014
classify <- function(
  x, # Vector with numeric values
  method="equalinterval", # type of binning or classification method (ways to get color class breakpoints)
  breaks, # specification for method
  Range=range(x, finite=TRUE), # Ends of color bar for method=equalinterval
  sdlab=1) # type for method=standarddeviation
{
x <- as.numeric(na.omit(x))
if(diff(range(x, finite=TRUE))==0) stop("all values are equal.")
# Partial matching of method:
PossibleValues <- c("equalinterval", "quantile", "usergiven", "standarddeviation")
method <- PossibleValues[pmatch(tolower(method),  PossibleValues)]
# actual work:
if(method=="equalinterval") # --------------------------------------------------
{
if(missing(breaks)) breaks <- 100     # default for breaks
nb <- breaks                          # number of bins (classes)
bb <- seqR(Range, length.out=nb+1)    # bin borders
at <- pretty2(bb)                     # position of labels in colPointsLegend / -Hist
la <- at                              # labels -"-
ix <- cut(x, breaks=bb, labels=FALSE, include.lowest=TRUE) # index of class for each value of x
} else if(method=="quantile") # ------------------------------------------------
{
if(missing(breaks)) breaks <- 0:4/4
if(!missing(breaks) & length(breaks)==1) breaks <- seq(0,1, length.out=breaks)
nb <- length(breaks) - 1
bb <- quantile(x, probs=breaks)
at <- bb
la <- signif(bb, 2) # rounding
ix <- cut(x, breaks=bb, labels=FALSE, include.lowest=TRUE)
} else if(method=="standarddeviation") # ---------------------------------------
{
if(missing(breaks)) breaks <- 3
breaks <- as.integer(breaks)
if(length(breaks)>1) {breaks <- breaks[1]; warning("breaks was vector. Only first element is used.")}
if(is.na(breaks)) stop("breaks must be an integer")
if(breaks <0) stop("breaks must be a positive integer >=1 if method=standarddeviation.")
if(sdlab==2|sdlab==4)
  {
  nb <- 2*breaks
  bb <- mean(x) + (-breaks:breaks)*sd(x)
  }
else
  {
  nb <- 2*breaks - 1
  bb <- mean(x) + (-breaks:(breaks-1)+0.5)*sd(x)
  }
at <- bb
if(sdlab==2)           {la <- paste(-breaks:breaks, "sd"); la[breaks+1] <- "m"} else
if(sdlab==3 | sdlab==4) la <- signif(at, 2) else
                        la <- paste(-breaks:(breaks-1)+0.5, "sd")
ix <- cut(x, breaks=bb, labels=FALSE)
} else if(method=="usergiven") # -----------------------------------------------
{
if(missing(breaks)) stop("breaks _must_ be specified if method is 'usergiven'.")
nb <- length(breaks) - 1
bb <- breaks
at <- bb
la <- signif(breaks, 2)
ix <- cut(x, breaks=bb, labels=FALSE)
} else  # ----------------------------------------------------------------------
stop("method can only be equalinterval, quantile, standarddeviation, or usergiven (but the name can be abbreviated).")
# Range Warning:
if(min(bb,na.rm=TRUE) > min(x) | max(bb,na.rm=TRUE) < max(x) )
   warning("There are values outside of the range of the given classes. These are given the index NA.")
# Results
list(nbins=nb, bb=bb, at=at, labels=la, index=ix)
} # Function end
