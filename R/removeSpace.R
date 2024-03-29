#' Remove white spaces from strings
#' 
#' Remove leading and/or trailing white space from character strings
#' 
#' @return Character string (vector)
#' @note If all arguments are FALSE, the string is returned unchanged.\cr Not
#' extensively tested yet, please mail me any problems...
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2014
#' @seealso \code{\link{sub}}, \code{\link{trimws}} since R 3.2.0 (April 2015)
#' @keywords character
#' @export
#' @examples
#' 
#' s <- c("space at end     ", "  white at begin", "  both ", " special ^  ")
#' removeSpace(s)
#' trimws(s)
#' 
#' # To add space, use:
#' x <- c("ab","abcde")
#' format(x)
#' format(x, justify="centre")
#' format(x, width=9)
#' 
#' @param x Character string, can be a vector
#' @param begin Logical. Remove leading spaces at the beginning of the character string? DEFAULT: TRUE
#' @param end Logical. Remove trailing spaces at the end? DEFAULT: TRUE
#' @param all Logical. Remove all spaces anywhere in the string? DEFAULT: FALSE
#' @param \dots Further arguments passed to \code{\link{sub}} or \code{\link{gsub}}, like \code{ignore.case, perl, fixed, useBytes}.
#' 
removeSpace <- function(
x,
begin=TRUE,
end=TRUE,
all=FALSE,
...
)
{
warning("since R 3.2.0 (April 2015), there is trimws(). removeSpace() will be removed from berryFunctions one day.")
if(all) gsub(pattern=" ", replacement="", x=x, ...) else
if(begin&end) sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE, ...) else
if(end) sub(" +$", '', x) else
if(begin) sub("^\\s+", "", x) else
x
}
