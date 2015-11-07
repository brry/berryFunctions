# give the structure of each dataset returned by \code{\link{data}}
# Berry Boessenkool, 2015-11-07 in search of good datasets for teaching

dataStr <- function(
package="datasets", # package
... # other arguments passed to \code{\link{data}}
)
{
d <- data(package=package, ...)$results[,"Item"]
d <- sapply(strsplit(d, split=" ", fixed=TRUE), "[", 1)
for(x in d){ message(x, ":  ", class(get(x))); message(str(get(x)))}
}
