# Remove leading and trailing white spaces
# Berry Boessenkool, Dec 2014
removeSpace <- function(
x, # Character string, can be a vector
begin=TRUE, # Logical. Remove leading spaces at the beginning of the character string?
end=TRUE, # Logical. Remove trailing spaces at the end?
all=FALSE, # Logical. Remove all spaces anywhere in the string?
... # Further arguments passed to \code{\link{sub}} or \code{\link{gsub}}, like \code{ignore.case, perl, fixed, useBytes}.
)
{
if(all) gsub(pattern=" ", replacement="", x=x, ...) else
if(begin&end) sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE, ...) else
if(end) sub(" +$", '', x) else
if(begin) sub("^\\s+", "", x) else
x
}

