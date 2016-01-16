# rescale vector: shift and scale values
# linear mapping onto a given range
# Berry Boessenkool, berry-b@gmx.de, Jan 2016

# see also: \code{scales::rescale}
# reference:


rescale <- function(
x,      # Numerical vector of values to be mapped to a given range
from=0, # output minimum
to=1    # output maximum
)
{
if(length(from)!=1) warning("from has length ", length(from))
if(length(to)!=1)   warning(  "to has length ", length(to))
  (x-min(x)) / (max(x)-min(x)) * (to - from) + from
}

