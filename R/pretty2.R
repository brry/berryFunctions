# Berry Boessenkool, Aug 2014
# pretty with no values outside of x range
pretty2 <- function(x, ...)
{
p <- pretty(x, ...)
r <- range(x, finite=TRUE)
p <- p[p>=r[1] & p<=r[2]]
p
}

