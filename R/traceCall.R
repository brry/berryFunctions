#' call stack of a function
#'
#' trace the call stack e.g. for error checking and format output for do.call levels
#'
#' @return Character string with the call stack
#' @section Warning: In \link{do.call} settings with large objects,
#'                   tracing may take a lot of computing time.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2016
#' @seealso \code{\link{checkFile}} for example usage
#' @keywords programming error
#' @importFrom utils capture.output
#' @export
#' @examples
#' lower <- function(a, s) warning(traceCall(s), "final value is: ", a+10)
#' upper <- function(b, skip=0) lower(b+5, skip)
#' upper(3)
#' upper(3, skip=1) # traceCall skips last level (warning)
#' upper(3, skip=4) # now the stack is empty
#' upper(3, skip=-1) # get one more level down
#' is.error(upper("four"))
#'
#' @param skip Number of levels to skip in \code{\link{traceback}}
#'
traceCall <- function(
skip=0
)
{
# the real skip value will be dependent on R version.
# since Feb 2016 (Version 3.3.0, May 2016),   .traceback(x)   is called in traceback(x),
# thus adding one more level to the call stack.
  realskip <- if(getRversion() < "3.3.0") 7+skip else 8+skip
  dummy <- capture.output(tb <- traceback(realskip) )
  # check for empty lists because skip is too large:
  if(length(tb)==0) return("\nCall stack: --empty-- \n")
  tb <- lapply(tb, "[", 1) # to shorten do.call (function( LONG ( STUFF)))
  tb <- lapply(tb, function(x) if(substr(x,1,7)=="do.call")
    sub(",", "(", sub("(", " - ", x, fixed=TRUE), fixed=TRUE) else x)
  calltrace <- sapply(strsplit(unlist(tb), "(", fixed=TRUE), "[", 1)
  calltrace <- paste(rev(calltrace), collapse=" -> ")
  calltrace <- paste0("\nCall stack: ", calltrace, "\n")
  calltrace
}
