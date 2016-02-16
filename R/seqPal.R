# sequential color palette ramp
# Berry Boessenkool, Jan 2016, berry-b@gmx.de

seqPal <- function(
n=12, # Number of colors
reverse=FALSE, # Reverse colors?
alpha=1, # Transparency (0=transparent, 1=fully colored)
extr=FALSE, # Should colors span possible range more extremely? If TRUE, it has very light yellow and very dark blue values included, using the result from \code{RColorBrewer::brewer.pal(9, "YlGnBu")}
yb=FALSE, # Should colors be in yellow-blue instead of the internal (nice) default?
yr=FALSE, # Should colors be in yellow-red instead of the default?
colors=NULL, # If not NULL, a color vector used in \code{\link{colorRampPalette}}
... # Further arguments passed to \code{\link{colorRamp}}
)
{
cols <- c("#FFFFC6","#CAE9AE","#85CDBA","#4DB6C6","#327EBD","#22329A")
if(extr) cols <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4",
                    "#1D91C0", "#225EA8", "#253494", "#081D58")
if(yb) cols <- c("yellow","blue")
if(yr) cols <- c("yellow","red")
if(!is.null(colors)) cols <- colors
if(reverse) cols <- rev(cols)
outcols <- colorRampPalette(cols)(n)
if(alpha!=1) outcols <- addAlpha(outcols, alpha)
outcols
}
