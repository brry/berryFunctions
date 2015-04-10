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
