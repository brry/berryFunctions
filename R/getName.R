#' get the name of an input in nested function calls
#' 
#' get the name of an input in nested function calls
#' 
#' @return Character string with the name
#' @author \url{https://stackoverflow.com/users/2725969/brodieg}
#'         Implementation Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2016
#' @seealso \url{https://stackoverflow.com/a/26558733}, \code{\link{substitute}}
#' @keywords character IO
#' @importFrom utils head
#' @export
#' @examples
#' # This does not work well:
#' 
#' lower <- function(x) deparse(substitute(x))
#' upper <- function(y) lower(y)
#' lower(pi) # returns "pi", as expected
#' upper(pi) # returns "y".
#' 
#' # That's why there is getName:
#' 
#' getName(pi) # returns "pi", as expected
#' upper <- function(y) getName(y)
#' upper(pi) # yay!
#' 
#' upper("dummy")
#' upper(dummy) # works also for nonexistent objects
#' dummy <- 7
#' upper("dummy") # still stable
#' upper(dummy) # still stable
#' 
#' upper(stackloss[1:5,])
#' 
#' upper2 <- function(data) upper(data)
#' upper2("K")
#' upper2(K)
#' 
#' # getName only works correctly if x is not an evaluated object:
#' lower2 <- function(inp, assign=FALSE) {if(assign) inp <- inp; getName(inp)}
#' lower2(pi)       # "pi"
#' lower2(pi, TRUE) # "3.14159265358979"
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
if(!is.character(var.name)) var.name <- deparse(var.name)
return(var.name)
}
