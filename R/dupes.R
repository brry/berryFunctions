# Number of duplicates per line of (text) file
# Per default saved to file which can be loaded into excel / libreoffice
# with conditional formatting of the first column, colors show for each line
# how often it occurs in the file.
# Note: OpenOffice does not provide color scales based on cell values.

# Berry boessenkool, 2014-12-19, berry-b@gmx.de

dupes <- function(
file,               # File name (character string)
ignore.empty=TRUE,  # Should empty lines be ignored?
ignore.space=TRUE,  # Should leading/trailing whitespace be ignored? 
tofile=missing(n),  # Logical: should output be directed to a file? Otherwise, a dataframe with line numbers and number of duplicates of that line will be printed in the console
n=length(d)         # Show only the first n values if \code{tofile=FALSE}
)
{
# empty lines or lines with only (up to 9) spaces:
spaces <- if(ignore.empty) sapply(0:9, function(i) 
                  paste(rep(" ", i), collapse="")) else FALSE
R <- readLines(file)
R2 <- if(ignore.space) removeSpace(R) else R
# indices of duplicated:
d <- which(duplicated(R2, incomparables=spaces, fromLast=TRUE) |
           duplicated(R2, incomparables=spaces, fromLast=FALSE)  )
# return n entries to console if tofile==F:
if(!tofile) {
nd <- sapply(d, function(i) sum(R2[i]==R2[-i]))
return(head(data.frame(line=d, number=nd), n))
}
# else:
nd <- sapply(1:length(R2), function(i) sum(R2[i]==R2[-i]))
if(ignore.empty) nd[R2==""] <- ""
write.table(data.frame(nd, R), paste(file,"_dupes.txt"), row.names=F, 
            col.names=F, quote=F, sep="\t")
message("Created the file '", file,"_dupes.txt'\nin getwd: ", getwd())
}
