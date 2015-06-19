# sort data.frame
# Berry Boessenkool, berry-b@gmx.de, 2015-06-19

sortDF <- function(
df, # Data.frame to be sorted
col, # Column (index or name) to be sorted by
... # Further arguments passed to \code{\link{order}}, like eg \code{decreasing=TRUE}
)
{
df[order(df[,col], ...),]
}
