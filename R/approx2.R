# fill NA values through advanced linear approximation
# Berry Boessenkool, berry-b@gmx.de, 2015-07-14

approx2 <- function(
x, # vector with values
fill=NULL # Function to fill NAs at the start or end of the vector. See Details.
)
{
# Input controls
if(all(is.na(x))) stop("There are no non-NA values in x.")
n <- length(x)
# Fill leading NAs (with the first available value):
if(is.na(x[1])) x[1] <- if(is.null(fill)) head(x[!is.na(x)],1) else fill(x, na.rm=TRUE)
# Fill trailing NAs (with the last available value):
if(is.na(x[n])) x[n] <- if(is.null(fill)) tail(x[!is.na(x)],1) else fill(x, na.rm=TRUE)
# Fill central NAs with a linear interpolation from the surrounding values:
approx(x, n=n)$y
}

