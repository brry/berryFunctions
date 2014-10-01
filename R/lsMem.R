# Show memory size of objects in MB
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# Berry, 22.02.2014
lsMem <- function(
                   n=6,
                   pos=1,
                   ...)
{
LS <- ls(pos=pos, ...)
if(n > length(LS))  n <- length(LS)
size <- sapply(LS, function(x) object.size(get(x)))
output <- sort(size/1e6, decreasing=TRUE)
if(n < length(LS)) c(output[1:n], "sum rest:"=sum(output[-(1:n)]))
else
output
}
