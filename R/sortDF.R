#' sort dataframes by column
#' 
#' sort a data.frame by column - basically just a wrapper for order
#' 
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June 2015
#' @seealso \code{\link{sort}}, \code{\link{order}}
#' @keywords univar manip arith
#' @export
#' @examples
#' 
#' sortDF(USArrests[USArrests$Murder>11,], "Assault")
#' sortDF(USArrests[USArrests$Murder>11,], 3)
#' 
#' @param df Data.frame to be sorted
#' @param col Column (index or (un)qouted name) to be sorted by
#' @param decreasing Logical: should highest value be on top?
#'                   DEFAULT: TRUE (unlike \code{\link{order}}!)
#' @param \dots Further arguments passed to \code{\link{order}}, like eg \code{na.last or method}
#' 
sortDF <- function(
df,
col,
decreasing=TRUE,
...
)
{
values <- getColumn(col,df)
df[order(values, decreasing=decreasing, ...),]
}
