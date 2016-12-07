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
#' middle <- function(b) {plot(b, main=b) ; warning("fake warning, b = ", b); lower(b) }
#' upper <- function(c) {cat("printing c:", c, "\n") ; middle(c)}
#' d <- upper(42)
#' d
#' rm(d)
#' 
#' \dontrun{ ## intentional error
#' d <- upper("42")                # error, no d creation 
#' traceback()                     # calling stack, but only in interactive mode
#' }
#' 
#' 
#' d <- try(upper("42"), silent=TRUE)      # d created
#' cat(d)                                  # has error message, but no traceback
#' inherits(d, "try-error")                # use for coding
#' 
#' 
#' d <- tryStack(upper("42"), silent=TRUE) # like try, but with traceback, even for warnings
#' cat(d)
#' cat(tryStack(upper("42"), silent=TRUE)) # level 1 different, but correct
#' 
#' d <- tryStack(upper("42"), silent=TRUE, tracewarnings=FALSE) # don't touch warnings
#' 
#' tryStack(upper(42)) # returns normal output, but warnings are easier to debug
#' 
#' stopifnot(inherits(d, "try-error"))
#' stopifnot(tryStack(upper(42))==52)
#' 
#' 
#' myfun <- function(k) tryStack(upper(k), silent=TRUE)
#' d <- myfun(42)
#' cat(d)
#' d <- myfun("42")
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
#' d <- tryStack(myfun(42) ) 
#' rm(myfun)
#' 
#' 
#' myfun1 <- function(k) try(upper(k), silent=TRUE)
#' d <- myfun1(42)
#' d <- myfun1("42") ; cat(d) # regular try output
#' myfun2 <- function(k) tryStack(myfun1(k), tracewarnings=FALSE, silent=TRUE)
#' d <- myfun2(42)
#' d <- myfun2("42")
#' cat(d) # empty stack - because of try, no real error happened
#' 
#' myfun1 <- function(k) tryStack(upper(k), silent=TRUE)
#' d <- myfun2("42")
#' cat(d)            # same result again! (ToDo: check if d[2] is the culprit)
#'  
#'
#' @param expr     Expresssion to try, potentially wrapped in curly braces if 
#'                 spanning several commands.
#' @param silent   Logical: Should error message + stack printing be suppressed?
#'                 DEFAULT: FALSE
#' @param tracewarnings Logical: Should warnings be traced as well?
#'                 They will still be printed as regular warnings, 
#'                 but with trace stack. DEFAULT: TRUE
#' @param file     File name passed to \code{\link{cat}}. 
#'                 If given, Errors will be appended to the file after two empty lines. 
#'                 if tracewarnings=TRUE and file!="", warnings will not be shown, 
#'                 but also appended to the file.
#'                 This is useful in lapply simulation runs.
#'                 DEFAULT: "" (catted to the console)
#' @param removetry Logical: should all stack entries matching typical tryCatch
#'                 expressions be removed? Unless the call contains customized
#'                 \code{\link{tryCatch}} code, this can be left to the DEFAULT: TRUE 
#'
tryStack <- function(
expr,
silent=FALSE,
tracewarnings=TRUE,
file="",
removetry=TRUE
)
{
# warnings to errors:
if(tracewarnings)
  {
  oop <- options(warn=-1)
  on.exit(options(oop))
  }
# environment for stack to (potentially) be written into:
tryenv <- new.env()
assign("stackmsg", value="-- empty stack --", envir=tryenv)
# error function
efun <- function(e, iswarning=FALSE)
  {
  # stack of calls, in case of an error:
  stack <- sys.calls()
  # remove the warning part:
  if(iswarning) stack <- head(stack, -8)
  # language to character:
  stack <- sapply(stack, deparse)
  # remove element from tryStack being in a function:
  toremovestring <- "withCallingHandlers(expr, error = efun, warning = wfun)"
  if(removetry) toremovestring <- c(toremovestring, 
                          "tryCatch(expr, error = function(e) {",
                          "tryCatchList(expr, classes, parentenv, handlers)",
                          "tryCatchOne(expr, names, parentenv, handlers[[1L]])",
                          "doTryCatch(return(expr), name, parentenv, handler)",
                          ".handleSimpleError(function (e, iswarning = FALSE)",
                          "h(simpleError(msg, call))",
                          "try(withCallingHandlers(expr, error = efun, warning = wfun),")
  toremove <- sapply(stack, function(x) any(grepl(
               berryFunctions::removeSpace(x[1]), toremovestring, fixed=TRUE)) )
  stack <- stack[!toremove]
  # combine vectors into a single string:
  stack <- lapply(stack, function(x) paste(x, collapse="\n"))
  # add error code:
  errorcode <- deparse(conditionCall(e))[1L]
  stack <- c(stack, errorcode)
  # add numbers:
  stack <- sapply(seq_along(stack), function(i) paste0(i, ": ", stack[[i]]))
  # add descriptor:
  info <- paste0("tryStack sys.calls() ", if(iswarning) "warning" else "error", " stack: ") 
  if(iswarning) info <- c(paste0("in ", errorcode, ": ", conditionMessage(e)), info)
  stack <- c(info, paste0("m: ", conditionMessage(e)), rev(stack))
  # add empty lines (-> line breaks -> readability), if file is given:
  if(file!="") stack <- c("","", stack, "")
  # put message into main function environment:
  assign("stackmsg", value=paste(stack,collapse="\n"), envir=tryenv)
  # print if not silent:
  shouldprint <- !silent && isTRUE(getOption("show.error.messages"))
  shouldprint <- shouldprint || file!=""
  if(shouldprint && !iswarning) 
    cat(tryenv$stackmsg, file=file, append=TRUE)
  # warn:
  if(iswarning) if(file!="") cat(tryenv$stackmsg, file=file, append=TRUE) else
                         warning(tryenv$stackmsg, immediate.=TRUE, call.=FALSE)
  }
# warning function
wfun <- function(e) efun(e, iswarning=TRUE)
if(!tracewarnings) wfun <- function(e){}
# now try the expression:
out <- try(withCallingHandlers(expr, error=efun, warning=wfun), silent=silent)
# add the trace stack character string to the output:
if(inherits(out, "try-error")) out[2] <- tryenv$stackmsg
# Done! return the output:
out
}

