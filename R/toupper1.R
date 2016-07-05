#' capitalize words
#'
#' capitalizes the first letter of character strings using \code{\link{toupper}}
#'
#' @details Basically just a one-liner using toupper
#'
#' @return character string vector
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2016
#' @seealso \code{\link{toupper}}, \code{\link{substr}}
#' @keywords character
#' @export
#' @examples
#' toupper1("berry")
#' toupper1(c("berRy","likes to code"))
#'
#' @param x Character vector
toupper1 <- function(
x
)
{
paste0(toupper(substr(x,1,1)), substr(x,2,nchar(x)))
}
