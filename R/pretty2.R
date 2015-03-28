# Berry Boessenkool, Aug 2014
# pretty with no values outside of x range
pretty2 <- function(x, n=5, force=FALSE, ...)
{
p <- pretty(x, n=n, ...)
r <- range(x, finite=TRUE)
p <- p[p>=r[1] & p<=r[2]]
if(force)                   # Added March 2015
  while(length(p) != n)
  {
  x <- extendrange(x, f=-0.01)
  p <- pretty(x, n=n, ...)
  p <- p[p>=r[1] & p<=r[2]]
  if(length(p) > n) p <- range(p)
  }
p
}

