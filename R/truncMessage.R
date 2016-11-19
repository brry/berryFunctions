#' truncate message parts
#'
#' truncate long vectors for messages
#'
#' @return Character string
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2016
#' @seealso \code{\link{message}}
#' @keywords character
# @importFrom package fun1 fun2
#' @export
#' @examples
#' truncMessage("hi")
#' truncMessage(paste0("hi",1:10))
#' truncMessage(paste0("hi",1:10), ntrunc=1)
#' truncMessage(paste0("hi",1:10), ntrunc=2, prefix="")
#' truncMessage(paste0("hi",1:10), ntrunc=8, prefix="files  ")
#' 
#' @param x Character vector
#' @param ntrunc Integer: number of elements printed before truncation. DEFAULT: 3
#' @param prefix Character: Prefix added if \code{length(x)>1}. DEFAULT: "s "
#' @param altnix Character: Alternative string padded around x if \code{length(x)==1}. 
#'               DEFAULT: "'"
#'
truncMessage <- function(
x,
ntrunc=3,
prefix="s ",
altnix="'"
)
{
l <- length(x)
if(l>ntrunc) x <- x[1:ntrunc]
paste0(if(l>1) prefix else altnix, toString(x), 
       if(l>ntrunc) paste(" (and",l-ntrunc,"more)"), if(l==1) altnix)
}
