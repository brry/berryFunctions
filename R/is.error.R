#' Check if an expression returns an error
#' 
#' Does a given expression return an error?
#' Useful for tests where you want to make sure your function throws an error.
#' 
#' @return TRUE/FALSE
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{stop}}, \code{\link{try}}, \code{\link{inherits}}
#' @keywords programming error
#' @export
#' @examples
#' is.error(  log(3)              )
#' is.error(  log("a")            )
#' is.error(  log(3),   tell=TRUE )
#' is.error(  log("a"), tell=TRUE )
#' stopifnot( is.error( log("a")  )  ) # or shorter:
#' is.error(  log("a"), force=TRUE)
#' # is.error(  log(3),   force=TRUE)
#' stopifnot(is.error(  is.error(log(3), force=TRUE)  ))
#' 
#' @param expr Expression to be tested for retunrning an error
#' @param tell Logical: Should the error message be printed via \code{\link{message}}? DEFAULT: FALSE
#' @param force Logical: Should an error be returned if the expression is not an error? DEFAULT: FALSE
#' 
is.error <- function(
expr,
tell=FALSE,
force=FALSE
)
{
expr_name <- deparse(substitute(expr))
test <- try(expr, silent=TRUE)
iserror <- inherits(test, "try-error")
if(tell) if(iserror) message("Note in is.error: ", test)
if(force) if(!iserror) stop(expr_name, " is not returning an error.", call.=FALSE)
# output:
iserror
}
