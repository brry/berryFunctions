#' try an expression, returning the error stack
#'
#' As in \code{\link{try}}, the result of an expression if it works.
#' If it fails, no error is thrown, but an invisible try-error class object is 
#' returned and a message \code{\link{cat}ted} to the console. Suppress the latter with silent=TRUE\cr
#' Unlike \code{\link{try}}, \code{tryStack} also returns the calling stack to ease debugging.
#'
#' @return Value of \code{expr} if evaluated successfully. If not, an invisible 
#' object of class "try-error" as in \code{\link{try}} with the stack in \code{object[2]}.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2016
#' @seealso \code{\link{try}}, \code{\link{traceCall}}
#' @keywords programming error
#' @export
#' @examples
#' 
#' lower <- function(a) a+10
#' middle <- function(b) {plot(b, main=b) ; lower(b) }
#' upper <- function(c) {cat("printing c:", c, "\n") ; middle(c)}
#' d <- upper(4)
#' d
#' rm(d)
#' 
#' is.error( d <- upper("4") , tell=TRUE)      # error, no d creation 
#' 
#' d <- try(upper("4"))
#' d
#' inherits(d, "try-error")
#' cat(d)
#' 
#' d <- tryStack(upper("4"))
#' d
#' inherits(d, "try-error")
#' cat(d)
#'
#'
#' @param expr Expresssion to try, potentially wrapped in curly braces if spanning several commands.
#' @param silent Logical: Should error message printing be suppressed?
#'
tryStack <- function(
expr,
silent=FALSE
)
{
# environment for stack to (potentially) be written into:
tryenv <- new.env()
# now try the expression:
out <- try(withCallingHandlers(expr, error=function(e)
  {
  # stack of calls, in case of an error:
  stack <- sys.calls()
  # remove the tryCatch part:
  stack <- stack[-(2:7)]
  # remove the current part:
  stack <- head(stack, -2)
  # language to character:
  stack <- sapply(stack, deparse)
  # print if not silent:
  if(!silent && isTRUE(getOption("show.error.messages"))) 
    cat("This is the error stack: ", stack, sep="\n")
  # put message into main function:
  assign("stackmsg", value=paste(stack,collapse="\n"), envir=tryenv)
  }), silent=silent)
# add the trace stack character string to the output:
if(inherits(out, "try-error")) out[2] <- tryenv$stackmsg
# Done! return the output:
out
}

