#' View values around an index
#'
#' View index rows of a data.frame with n surrounding rows
#'
#' @return Nothing, calls \code{\link{View}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2016
#' @seealso \code{\link{sortDF}}, \code{\link{View}}
#' @importFrom utils View
#' @export
#' @examples
#' \dontrun{ ## View should not be used in examples
#' myDF <- data.frame(A=1:30, B=cumsum(rnorm(30)))
#' myDF[c(5,7,23,29),1] <- NA
#' around(myDF, i=is.na(myDF$A))
#' around(myDF, i=c(11,19), n2=0)
#' }
#'
#' @param x Data.frame
#' @param i Index (logical or integers)
#' @param n1 Number of elements shown before each i. DEFAULT: 2
#' @param n2 Number of elements shown after each i. DEFAULT: n1
#' @param convert Use \code{\link{which}} to get the row numbers?
#'                DEFAULT: TRUE if i is boolean
#' 
around <- function(
x,
i,
n1=2,
n2=n1,
convert=is.logical(i)
)
{
if(convert) i <- which(i)
index <- lapply(i, "+", -n1:n2)
index <- unique(unlist(index))
index <- index[index>0 & index<NROW(x)]
View(x[index,], title=paste0(deparse(substitute(x)), "_around"))
}
