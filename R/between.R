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
#' between(1:10, 4:8)
#' between(1:10, 8, 4) # warns about interval
#'  
#' data.frame( incl.T=between(1:10, 4, 8), 
#'             incl.F=between(1:10, 4, 8,  incl=FALSE),
#'            aincl.F=between(1:10, 4, 8, aincl=FALSE),
#'            bincl.F=between(1:10, 4, 8, bincl=FALSE)  )
#' 
#' @param x    Numerical vector
#' @param a,b  Numerical values/vectors specifying the borders of the interval. 
#' @param incl Logical. Include values on the borders? For x == border, TRUE
#'             will be returned. Specify per left and right border separately
#'             with the arguments \code{aincl} and \code{bincl}. DEFAULT: TRUE
#' @param aincl,bincl Logical. Include values on left and right border, respectively?
#'                    DEFAULT: \code{incl}
#'
between <- function(
x,
a,
b=a,
incl=TRUE,
aincl=incl,
bincl=incl
)
{
b <- max(b, na.rm=TRUE)
a <- min(a, na.rm=TRUE)
if(b<a) warning("b (",b,") is larger than a (",a,"), results may be not as intended.")
aa <- if(aincl) x >= a else x > a
bb <- if(bincl) x <= b else x < b
aa & bb
}
