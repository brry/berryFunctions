# insert Rows to data.frame
# Basic (computationally quick) solution by Ari B. Friedmann
# http://stackoverflow.com/questions/11561856/add-new-row-to-dataframe
# for loop, recycling, input controls and data.framification added by
# Berry Boessenkool, 2015-10-22, berry-b@gmx.de

insertRows <- function(
df,    # data.frame
r,     # Row number (not name!), at which the \code{new} row is to be inserted. Can be a vector
new=NA # Vector with data to be inserted, is recycled. Alternatively, a data.frame, whose rows are put into the r locations. If it has more rows than length(r), the excess rows are ignored
)
{
# Input checks:
if(!is.data.frame(df)) warning("df is not a data.frame.")
if(!(is.vector(new) | is.data.frame(new))) warning("new row is not a vector or data.frame.")
# recycle new row values:
if(!is.data.frame(new))
  {
  new <- rep(new, length=ncol(df)) # recycle vector to number of columns
  new <- rbind(new) # make it into a data.frame (well, a matrix, but that works fine, too)
  }
# recycle the rows:
new <- new[rep(1:nrow(new), length=length(r)), , drop=FALSE]
# for each value in r, create a row with the i-th row of new:
for(i in 1:length(r))
  {
  SEQ <- seq(from=r[i], to=nrow(df))
  df[SEQ+1,] <- df[SEQ,]
  df[r[i],] <- new[i,]
  }
# return the final output:
df
}
