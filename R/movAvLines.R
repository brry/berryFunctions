# Lines with moving averages
# Berry Boessenkool, May 20, 2015    berry-b@gmx.de

movAvLines <- function(
y,             # y values that are smoothed with several window widths
x=1:length(y), # x values of data
widths=2:7*2-1,# widths of \code{\link{movAv}} windows
weights,       # weights within each window
col="blue",    # color passed to \code{\link{addAlpha}}
alpha=0.3,     # transparency passed to \code{\link{addAlpha}}
plot=FALSE,    # should scatterplot be created first?
las=1,         # LabelAxisStyle (only relevant if plot=TRUE)
...            # further arguments passed to \code{\link{lines}}
)
{
if(plot) plot(x,y, las=las)
for(i in 1:length(widths))
   lines(x, movAv(y, width=widths[i], weights=weights), col=addAlpha(col, alpha), ...)
}

