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
#' 
#'
#' @param \dots Currently ignored
#'
traceCall <- function(
...
)
{
  dummy <- capture.output(tb <- traceback(8) )
  tb <- lapply(tb, "[", 1) # to shorten do.call (function( LONG ( STUFF)))
  tb <- lapply(tb, function(x) if(substr(x,1,7)=="do.call")
    sub(",", "(", sub("(", " - ", x, fixed=TRUE), fixed=TRUE) else x)
  calltrace <- sapply(strsplit(unlist(tb), "(", fixed=TRUE), "[", 1)
  calltrace <- paste(rev(calltrace), collapse=" -> ")
  calltrace <- paste0("\nCall stack: ", calltrace, "\n")
}
