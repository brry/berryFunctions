#' Prepend spaces before na.strings
#' 
#' Returns a number of useful character strings with varying amount of spaces prepended. 
#' It can be used as \code{na.strings=na9()} in \code{\link{read.table}}.
#' 
#' @return Character strings
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2016
#' @seealso \code{\link{paste}}
#' @keywords IO file
#' @export
#' @examples
#' 
#' na9()
#' na9(nspace=0, sep=".")
#' na9(nspace=0, sep=".", more=c(NA,"-"))
#'
#' @param nspace number of spaces prepended. DEFAULT: 5
#' @param base Numeric: basic na.string numbers
#' @param sep Separator string (comma or decimal point or both). DEFAULT: c(",",".")
#' @param digits Number(s) of zeros to be appended. DEFAULT: 0:4
#' @param more More structures added to base, like "NA", "--". digits and sep is not added to this! DEFAULT: NULL
#' @param \dots Arguments passed to nothing currently
#' 
na9 <- function(
nspace=5,
base=c(-9999,-999, -9.99, -9.999),
sep=c(",","."),
digits=0:4,
more=NULL,
...)
{
# zeros:
base <- c(sapply(digits, function(d) sapply(base, format, nsmall=d)   ))
# separator signs:
base <- c(sapply(sep, function(s) gsub(".", s, base, fixed=TRUE) ))
# spaces:
spaces <- sapply(0:nspace, function(i) paste(rep(" ",i), collapse=""))
base <- paste0(rep(spaces, each=length(base)), base)
# add more:
base <- c(base, more)
# output:
unique(base)
}
