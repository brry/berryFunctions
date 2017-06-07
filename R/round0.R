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
#'
#' @param x      Values
#' @param digits Number of digits to keep. DEFAULT: 2
#' @param width  Total width (number of characters including dot). DEFAULT: digits+2
#' @param flag   Flag. DEFAULT: "0"
#' @param \dots  Further arguments passed to \code{\link{formatC}},
#'               except for "format".
#'
round0 <- function(
x,
digits=0,
width=digits+ifelse(digits==0,2,3),
flag=0,
...
)
{
formatC(round(x,digits), format='f', digits=digits, width=width, flag=flag, ...)
}
