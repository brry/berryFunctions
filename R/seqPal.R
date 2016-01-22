# sequential color palette ramp
# Berry Boessenkool, Jan 2016, berry-b@gmx.de

seqPal <- function(
n=12, # Number of colors
reverse=FALSE, # Reverse colors?
alpha=1, # Transparency (0=transparent, 1=fully colored)
yb=FALSE, # Should colors be in yellow-blue instead of yellow-red?
colors=NULL, # If not NULL, a color vector used in \code{\link{colorRampPalette}}
... # Further arguments passed to \code{\link{colorRamp}}
)
{
cols <- c("yellow","red")
if(yb) cols <- c("yellow","blue")
if(!is.null(colors)) cols <- colors
if(reverse) cols <- rev(cols)
outcols <- colorRampPalette(cols)(n)
if(alpha!=1) outcols <- addAlpha(outcols, alpha)
outcols
}
