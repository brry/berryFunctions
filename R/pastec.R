# Helper function paste with collapes=", "
# Berry Boessenkool, 2015-04-10
pastec <- function(
...,          # Object(s) to be \code{\link{paste}d} to a character vector
sep=" ",      # Character string to separate single strings
collapse=", " # Character string between combined strings
)
{
paste(..., sep=sep, collapse=collapse)
}

## Old idea to enable newline insertion every n elements, apparently not necessary.
##newlines=TRUE # try to add line breaks at suitable intervals
##\item{newlines}{Add line breaks? (ca. at console width). DEFAULT: TRUE}
##dd <- paste(..., sep=sep, collapse=collapse)
##if(newlines)
##{
##  w <- getOption("width")
##  if(nchar(dd) < w) return(dd) 
##  dd2 <- strsplit(dd, collapse)[[1]]
##  sel <- cumsum(nchar(dd2)+2) < w
##  dd3 <- dd2[sel]
##}
##return(dd)
