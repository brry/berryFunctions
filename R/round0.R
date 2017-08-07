#' Round numbers with leading and trailing zeros
#' 
#' Round numbers and add leading + trailing zeros
#' 
#' @return Character string vector
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{formatC}}, \code{\link{sprintf}}
#' @export
#' @examples
#' round0( pi*10^(-3:5), 2)
#' stopifnot(round0(17.3, 2) == "17.30")
#' round0(7.3)
#' round0(7.3, 2)
#' round0(c(0.2,7.3,12.8), 2, pre=1)
#' round0(c(0.2,7.3,12.8), 1, pre=3, flag="") # spaces instead of zeros
#' 
#' @param x      Value(s)
#' @param digits Number of digits (after decimal separator) to keep. DEFAULT: 0
#' @param pre    Number of characters before the decimal separator. DEFAULT: 2
#' @param width  Total width (number of characters including dot).
#'               DEFAULT: digits+pre (+1 if needed)
#' @param flag   Flag. Could be "" for spaces. DEFAULT: "0"
#' @param \dots  Further arguments passed to \code{\link{formatC}},
#'               except for "format".
#' 
round0 <- function(
x,
digits=0,
pre=2,
width=digits+pre+ifelse(digits==0,0,1),
flag=0,
...
)
{
formatC(round(x,digits), format='f', digits=digits, width=width, flag=flag, ...)
}
