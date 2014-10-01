# simple Function to add n rows to a data.frame
# Berry, 9.1.2014

addRows <- function(
          df,
          n,
          values=NA)
{
dnew <- data.frame(matrix(values, nrow=n, ncol=ncol(df)))
colnames(dnew) <- colnames(df)
rbind(df, dnew)
}
