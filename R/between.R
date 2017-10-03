#' @title Are values between a and b?
#' @description Are values within a certain interval? 
#' Basically a wrapper for \code{ x >= a   &   x <= b } to save repeating long x names twice.
#' @return Logical (boolean) vector with TRUE/FALSE values
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @seealso \code{\link{findInterval}}
# \code{graphics::\link[graphics]{plot}}
#' @keywords logic
# @importFrom package fun1 fun2
#' @export
#' @examples
#' between(1:10, 4, 8)
#' between(1:10, 4:8) # range as vector
#' between(1:10, 8, 4) # warns about interval
#'  
#' data.frame( incl.T=between(1:10, 4, 8), 
#'             incl.F=between(1:10, 4, 8,  incl=FALSE),
#'            aincl.F=between(1:10, 4, 8, aincl=FALSE),
#'            bincl.F=between(1:10, 4, 8, bincl=FALSE)  )
#' 
#' @param x    Numerical vector
#' @param a,b  Numerical values/vectors specifying the borders of the interval. 
#'             \code{\link{min}} and \code{max} are used, so they can be a vector.
#' @param incl Logical. Include values on the borders? For x == border, TRUE
#'             will be returned. Specify per left and right border separately
#'             with the arguments \code{aincl} and \code{bincl}. DEFAULT: TRUE
#' @param aincl,bincl Logical. Include values on left and right border, respectively?
#'                    DEFAULT: \code{incl}
#' @param quiet Logical. Suppress warning if a>b? DEFAULT: FALSE
#' 
between <- function(
x,
a,
b=a,
incl=TRUE,
aincl=incl,
bincl=incl,
quiet=FALSE
)
{
b <- max(b, na.rm=TRUE)
a <- min(a, na.rm=TRUE)
if(a>b&!quiet) warning("a (",a,") is larger than b (",b,
                       "), results may differ from intention.")
aa <- if(aincl) x >= a else x > a
bb <- if(bincl) x <= b else x < b
aa & bb
}
