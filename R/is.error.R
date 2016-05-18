#' Check if an expression returns an error
#'
#' Does a given expression return an error?
#'
#' @return TRUE/FALSE
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{stop}}, \code{\link{try}}, \code{\link{inherits}}
#' @keywords programming error
#' @export
#' @examples
#' is.error(  log(3)              )
#' is.error(  log("a")            )
#' is.error(  log("a"), tell=TRUE )
#' stopifnot( is.error( log("a")  )  )
#'
#' @param expr Expression to be tested for retunrning an error
#' @param tell Logical: Should the error message be printed via \code{\link{message}}? DEFAULT: FALSE
#'
is.error <- function(
expr,
tell=FALSE
)
{
test <- try(expr, silent=TRUE)
if(tell) message("Note in is.error: ", test)
inherits(test, "try-error")
}
