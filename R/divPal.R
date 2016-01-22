# palette color Ramp from IPCC Assesment Report 5 Figure 12.22
# Berry Boessenkool, Jan 2016, berry-b@gmx.de
# diverging palette, good for displaying values in two directions
# Brown to blue in originally 12 shades


divPal <- function(
n=12, # Number of colors
reverse=FALSE, # Reverse colors?
alpha=1, # Transparency (0=transparent, 1=fully colored)
rwb=FALSE, # Should colors be in red-white-blue instead of brown-blue?
colors=NULL, # If not NULL, a color vector used in \code{\link{colorRampPalette}}
... # Further arguments passed to \code{\link{colorRamp}}
)
{
cols <- c("#9B5523", "#B16A32", "#CA8448", "#F4C882", "#F1DB99", "#FBF5B4",
          "#C3E2C0", "#96D1A7", "#46BEA0", "#4984A0", "#4984A0", "#0B3A5B")
if(rwb) cols <- c("red","white","blue")
if(!is.null(colors)) cols <- colors
if(reverse) cols <- rev(cols)
outcols <- colorRampPalette(cols)(n)
if(alpha!=1) outcols <- addAlpha(outcols, alpha)
outcols
}
