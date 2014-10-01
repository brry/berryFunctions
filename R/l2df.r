# list to data.frame:
# convert list with vectors of unequal length to dataframe, pad with NAs
# Berry Boessenkool, Jan 2014

l2df <- function(
                 list,
                 byrow=TRUE)
{
maxlen <- max(sapply(list,length))
df <- sapply(list, "[", 1:maxlen) # apply the indexing function to each element
if(byrow) t(df) else df
}
