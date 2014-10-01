# compare two files. Returns line numbers with differences.
# Berry Boessenkool, Aug. 2014
compFiles <- function(
file1, file2, # Filenames to be read by \code{\link{readLines}}
nr=20, # number of results printed
startline=1, # start and end lines, e.g. to exclude section that is already compared
endline=length(f1),
quiet=FALSE, # show warnings about file lengths?
... # further arguments passed to \code{\link{readLines}}
)
{
f1 <- readLines(file1, ...)
f2 <- readLines(file2, ...)
# truncate:
if(length(f1) > length(f2) )
   {
   if(!quiet) warning(length(f1) - length(f2), " lines are discarded at the end of the file ", file1, ", as ", file2, " is shorter.")
   f1 <- f1[1:length(f2)]
   }
   if(length(f2) > length(f1) )
   {
   if(!quiet) warning(length(f2) - length(f1), " lines are discarded at the end of the file ", file2, ", as ", file1, " is shorter.")
   f2 <- f2[1:length(f1)]
   }
# truncate:
f1 <- f1[startline:endline]
f2 <- f2[startline:endline]
# compare
head( which(f1 != f2)+startline-1 , nr)
}
