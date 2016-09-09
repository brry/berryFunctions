#' get the name of an input in nested function calls
#'
#' get the name of an input in nested function calls
#'
#' @return Character string with the name
#' @author \url{http://stackoverflow.com/users/2725969/brodieg}
#'         Implementation Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2016
#' @seealso \url{http://stackoverflow.com/a/26558733}, \code{\link{substitute}}
#' @keywords character IO
#' @importFrom utils head
#' @export
#' @examples
#' getName(pi) # returns "pi", as expected
#' upper <- function(y) getName(y)
#' upper(pi) # yay!
#'
#' # This does not work with
#' lower <- function(x) deparse(substitute(x))
#' upper <- function(y) lower(y)
#' lower(pi) # returns "pi", as expected
#' upper(pi) # returns "y".
#'
#' @param x input object name or character string
#'
getName <- function(x)
{
my.call <- quote(substitute(x))
var.name <- eval(my.call)
for(i in rev(head(sys.frames(), -1L)))
  {
  # First frame doesn't matter since we already substituted for first level,
  # reverse order of sys.frames (which is in order of evaluation)
  my.call[[2]] <- var.name    # replace with the modified variable
  var.name <- eval(my.call, i)
  }
return(as.character(var.name))
}
