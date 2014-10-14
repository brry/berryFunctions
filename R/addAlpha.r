
# Make existing colors transparent (add alpha)
# Berry Boessenkool, June 2014
# Based on suggestion by Mathias Seibert, Dec. 2013

addAlpha <- function(
  col, # vector of color names (\code{\link{colors}) that can be interpreted by \code{\link{col2rgb}}
  alpha=0.3 # Level of semi-transparency. between 0 (transparent) and 1 (intransparent).
  )
{
rgb2 <-  function(x) rgb(x[1], x[2], x[3], alpha=alpha)
output <- apply(X=sapply(col,col2rgb)/255, MARGIN=2, FUN=rgb2)
if( length(alpha)==1 | length(col)==1)  return(as.vector(output)) else
if( length(alpha) == length(col) ) return(diag(output)) else
warning("col and alpha had different lengths"); return(output)
}
