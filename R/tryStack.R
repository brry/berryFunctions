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
#' @seealso \code{\link{try}}, \code{\link{traceCall}},
#' \url{http://r.789695.n4.nabble.com/Stack-trace-td4021537.html},
#' \url{http://stackoverflow.com/questions/15282471/get-stack-trace-on-trycatched-error-in-r},
#' \url{http://stackoverflow.com/questions/1975110/printing-stack-trace-and-continuing-after-error-occurs-in-r},
#' \url{http://stackoverflow.com/questions/16879821/save-traceback-on-error-using-trycatch}
#' 
#' @keywords programming error
#' @export
#' @examples
#' 
#' lower <- function(a) a+10
#' middle <- function(b) {plot(b, main=b) ; lower(b) }
#' upper <- function(c) {cat("printing c:", c, "\n") ; middle(c)}
#' d <- upper(42)
#' d
#' rm(d)
#' 
#' is.error( d <- upper("42") , tell=TRUE)      # error, no d creation 
#' 
#' d <- try(upper("42"))
#' d
#' inherits(d, "try-error")
#' cat(d)
#' 
#' d <- tryStack(upper("42"))
#' d
#' inherits(d, "try-error")
#' cat(d)
#' cat(tryStack(upper("42")))
#' 
#' stopifnot(tryStack(upper(42))==52)
#' 
#' 
#' myfun <- function(k) tryStack(upper("42"))
#' d <- myfun(42)
#' cat(d)
#' 
#' myfun <- function(k) tryStack(stop("oh oh"))
#' d <- myfun(42)
#' 
#' d <- tryStack(myfun(4) ) # nested calls get weird
#' cat(d)                   # They make that empty in the second run, for example.
#'
#' myfun <- function(k) cat( tryStack(upper("42")) )
#' myfun(42) 
#' d <- tryStack(myfun(42) ) # or give the complete try call...
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
assign("stackmsg", value="-- empty stack --", envir=tryenv)
# now try the expression:
out <- try(withCallingHandlers(expr, error=function(e)
  {
  # stack of calls, in case of an error:
  stack <- sys.calls()
 # browser()
  # remove the tryCatch part:
  stack <- stack[-(2:7)]
  # remove the current part:
  stack <- head(stack, -2)
  # language to character:
  stack <- sapply(stack, deparse)
  # remove element from tryStack being in a function:
  toremove <- "withCallingHandlers(expr, error = function(e) {    stack <- sys.calls()"
  removetoo <- sapply(stack, function(x) grepl(toremove, paste(x[1:2],collapse=""), fixed=TRUE))
  stack <- stack[!removetoo]
  # add error code:
  stack <- c(stack, deparse(conditionCall(e))[1L])
  # add numbers:
  stack <- sapply(seq_along(stack), function(i) paste0(i, ": ", stack[[i]]))
  # add descriptor:
  stack <- c("tryStack sys.calls() error stack: ", stack)
  # put message into main function environment:
  assign("stackmsg", value=paste(stack,collapse="\n"), envir=tryenv)
  # print if not silent:
  if(!silent && isTRUE(getOption("show.error.messages"))) 
    cat(tryenv$stackmsg, sep="\n")
  }), silent=silent)
# add the trace stack character string to the output:
if(inherits(out, "try-error")) out[2] <- tryenv$stackmsg
# Done! return the output:
out
}

