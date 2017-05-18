#' try an expression, returning the error stack
#'
#' As in \code{\link{try}}, the result of an expression if it works.
#' If it fails, execution is not halted, but an invisible try-error class object is 
#' returned and (unless silent=TRUE) a message \code{\link{cat}ted} to the console.\cr
#' Unlike \code{\link{try}}, \code{tryStack} also returns the calling stack to 
#' trace errors and warnings and ease debugging.
#'
#' @return Value of \code{expr} if evaluated successfully. If not, an invisible 
#' object of class "try-error" as in \code{\link{try}} with the stack in \code{object[2]}.
#' For nested tryStack calls, \code{object[3], object[4]} etc. will contain "-- empty error stack --"
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
#' # Functions -----
#'
#' lower <- function(a) {message("fake message, a = ", a); a+10}
#' middle <- function(b) {plot(b, main=b) ; warning("fake warning, b = ", b); lower(b) }
#' upper <- function(c) {cat("printing c:", c, "\n") ; middle(c)}
#' d <- upper(42)
#' d
#' rm(d)
#' 
#' 
#' # Classical error management with try -----
#' 
#' \dontrun{ ## intentional error
#' d <- upper("42")                # error, no d creation 
#' traceback()                     # calling stack, but only in interactive mode
#' }
#' 
#' d <- try(upper("42"), silent=TRUE)      # d created
#' cat(d)                                  # with error message, but no traceback
#' inherits(d, "try-error")                # use for coding
#' 
#' 
#' # way cooler with tryStack -----
#' 
#' d <- tryStack(upper("42") ) # like try, but with traceback, even for warnings
#' cat(d)
#' d <- tryStack(upper("42"), silent=TRUE, warn=0) # don't trace warnings
#' d <- tryStack(upper("42"), short=FALSE)
#'  
#' tryStack(upper(42)) # returns normal output, but warnings are easier to debug
#' # Note: you can also set options(showWarnCalls=TRUE)
#' 
#' stopifnot(inherits(d, "try-error"))
#' stopifnot(tryStack(upper(42))==52)
#' 
#' \dontrun{ ## file writing not wanted by CRAN checks
#' d <- tryStack(upper("42"), silent=TRUE, file="log.txt")
#' system2("open", "log.txt") # on linux, try xdg-open
#' unlink("log.txt")
#' }
#' 
#' op <- options(warn=2)
#' d <- try(upper("42") )
#' cat(d)
#' d <- tryStack(upper("42") )
#' d <- tryStack(upper("42"), warn=FALSE)
#' cat(d)
#' options(op) ; rm(op)
#' 
#' # Nested calls -----
#' 
#' f <- function(k) tryStack(upper(k), silent=TRUE)
#' d <- f(42)                 ; cat("-----\n", d, "\n-----\n") ; rm(d)
#' d <- f("42")               ; cat("-----\n", d, "\n-----\n") ; rm(d)
#' d <- tryStack(f(4) )       ; cat("-----\n", d, "\n-----\n") ; rm(d) 
#' # warnings in nested calls are printed twice, unless warn=0
#' d <- tryStack(f(4), warn=0) # could also be set within 'f'
#' 
#' d <- tryStack(f("4"))      ; cat("-----\n", d, "\n-----\n") 
#' d[1:3] ; rm(d)
#' # empty stack at begin - because of tryStack in f, no real error happened in f
#' 
#' 
#' # Other tests -----
#' 
#' cat( tryStack(upper("42")) )
#' f <- function(k) tryStack(stop("oh oh"))
#' d <- f(42) ; cat("-----\n", d, "\n-----\n") ; rm(d) # level 4 not helpful, but OK
#' 
#' # stuff with base::try
#' f <- function(k) try(upper(k), silent=TRUE)
#' d <- f(42)     ; cat("-----\n", d, "\n-----\n") ; rm(d)
#' d <- f("42")   ; cat("-----\n", d, "\n-----\n") ; rm(d) # regular try output
#' 
#' f2 <- function(k) tryStack(f(k), warn=0, silent=TRUE)
#' d <- f2(42)    ; cat("-----\n", d, "\n-----\n") ; rm(d)
#' d <- f2("42")  ; cat("-----\n", d, "\n-----\n") ; rm(d) # try -> no error. 
#' # -> Use tryCatch and you can nest those calls. note that d gets longer.
#' 
#'
#' @param expr     Expresssion to try, potentially wrapped in curly braces if 
#'                 spanning several commands.
#' @param silent   Logical: Should printing of error message + stack be suppressed?
#'                 Does not affect warnings and messages. DEFAULT: FALSE
#' @param warn     Logical: trace \code{\link{warning}s} and \code{\link{message}s} also?
#'                 They are still handled like regular warnings / messages unless 
#'                 \code{file !=""}, when they are catted into that file. DEFAULT: TRUE
#' @param short    Logical: should trace be abbreviated to upper -> middle -> lower?
#'                 If NA, it is set to TRUE for warnings and messages, FALSE for errors.
#'                 DEFAULT: TRUE
#' @param file     File name passed to \code{\link{cat}}. 
#'                 If given, Errors will be appended to the file after two empty lines. 
#'                 if \code{warn=T} and file!="", warnings and messages will not be shown, 
#'                 but also appended to the file.
#'                 This is useful in lapply simulation runs.
#'                 DEFAULT: "" (catted to the console)
#' @param removetry Logical: should all stack entries matching typical tryCatch
#'                 expressions be removed? Unless the call contains customized
#'                 \code{\link{tryCatch}} code, this can be left to the DEFAULT: TRUE 
#' @param skip     Character string(s) to be removed from the stack.
#'                 e.g. "eval(expr, p)". Use short=F to find exact matches.
#'                 DEFAULT: NULL
tryStack <- function(
expr,
silent=FALSE,
warn=TRUE,
short=TRUE,
file="",
removetry=TRUE,
skip=NULL
)
{
# silence warnings:
if(warn)
  {
  oop <- options(warn=-1)
  on.exit(options(oop))
  }
if(is.na(file)) file <- ""
# environment for stack to (potentially) be written into:
tryenv <- new.env()
assign("emsg", value="-- empty error stack --"  , envir=tryenv)
assign("wmsg", value="-- empty warning stack --", envir=tryenv)
assign("mmsg", value="-- empty message stack --", envir=tryenv)

# strings that will be removed from stack (if matching exactly):
toremovestring <- "try(withCallingHandlers(expr, error = efun, warning = wfun, message = mfun),"
toremovestring <- c(toremovestring, skip)
if(removetry) toremovestring <- c(toremovestring,
                     "tryCatch(expr, error = function(e) {",
                     "tryCatchList(expr, classes, parentenv, handlers)",
                     "tryCatchOne(expr, names, parentenv, handlers[[1L]])",
                     "doTryCatch(return(expr), name, parentenv, handler)",
                     ".handleSimpleError(function (e)",
                     "h(simpleError(msg, call))",
                     ".handleSimpleError(function (e, type = \"error\")",
                     "withRestarts({"
                     )


# error/warning/message function
efun <- function(e, type="error")
{
# don't touch warnings/messages if warn=FALSE:
if(type!="error") if(!warn) return() 
  
# stack of calls, only determined in case of an error/warning/message:
stack <- sys.calls()
# language to character:
stack <- lapply(stack, deparse)

# remove the warning part:
if(type=="warning" | type=="message") 
  {
  stack <- head(stack, -5) # also for message?
  # usually, two more need to be removed:
  if(any(grepl(".signalSimpleWarning(", stack[[length(stack)]], fixed=TRUE)))
    stack <- head(stack,-1)
  # remove recursive warning part:
  irecwarn <- grep(".signalSimpleWarning(", stack, fixed=TRUE)
  if(length(irecwarn)>0) if(stack[irecwarn+2] == "withOneRestart(expr, restarts[[1L]])")
      stack <- stack[-(irecwarn+0:2)]
  
  }
stack <- stack[stack!="doWithOneRestart(return(expr), restart)"]

# remove common try elements from tryStack:
toremove <- sapply(stack, function(x) any(grepl(removeSpace(x[1]), toremovestring, fixed=TRUE)) )
stack <- stack[!toremove]
# remove tryCatch elements
if(removetry)
  {
  toremove2 <- sapply(stack, function(x) substr(x[1],1,9)=="tryCatch(") # this one may remove too much
  stack <- stack[!toremove2]
  }

# combine vectors into a single string:
stack <- lapply(stack, function(x) paste(x, collapse="\n"))

# shorten stack, keep only function names:
if(is.na(short)) short <- type!="error"
if(short)
  {
  # shorten do.call (function( LONG ( STUFF)))
  stack <- lapply(stack, function(x) if(substr(x,1,7)=="do.call") 
               sub(",", "(", sub("(", " - ", x, fixed=TRUE), fixed=TRUE) else x)
  # keep try calls more informative:
  stack <- lapply(stack, function(x) if(substr(x,1,4)=="try(") 
               sub("(", " - ", x, fixed=TRUE) else x)  
  stack <- lapply(strsplit(unlist(stack), "(", fixed=TRUE), "[", 1)
}

# add code that produced error/warning/message:
ccall <- deparse(conditionCall(e))[1L]
stack <- c(stack, ccall)
# add numbers:
if(!short) stack <- lapply(seq_along(stack), function(i) paste0(i, ": ", stack[[i]]))

# concatenate:
stack <- if(short) paste(    stack , collapse=" -> ") else 
                   paste(rev(stack), collapse="\n"  )

# condition message:
cmes <- conditionMessage(e)
# catch nested (recursive) calling:
cmes <- strsplit(cmes, "tryStack sys.calls", fixed=TRUE)[[1]][1]
# remove end line breaks
while(substring(cmes,nchar(cmes))=="\n") cmes <- substring(cmes,1,nchar(cmes)-1)

if(!short) stack <- paste0("m: ", cmes, "\n", stack)


# additional information (line breaks and sys.time) if file is given:
prefix <- suffix <- ""
if(file!="") 
  {
  prefix <- paste0("\n---------------\n", as.character(Sys.time()),
                   "\n\n", type, if(short) ":" , " ")
  suffix <- "\n"
  }

# short or long informational description:
info <- ""
if(type=="warning" | type=="error") info <- paste0("in ", ccall, ": ")
info <- paste0(info, cmes, "\n-- tryStack sys.calls")
info <- paste0(info, if(short) ": " else "\n")

# put message into main function environment: emsg/wmsg/mmsg
assign(x=paste0(substr(type,1,1),"msg"), 
       value=paste0(prefix, if(type!="error"||file!="")info, stack, suffix), 
       envir=tryenv)

# generate warning / message / error:
if(type=="warning") 
  {
  if(file=="") warning(tryenv$wmsg, immediate.=TRUE, call.=FALSE) 
  else             cat(tryenv$wmsg, file=file, append=TRUE)
  }
if(type=="message") 
  {
  if(file=="") message("Message: ", tryenv$mmsg, appendLF=TRUE)
  else             cat(tryenv$mmsg, file=file, append=TRUE)
  invokeRestart("muffleMessage")
  }
if(type=="error") 
  {
  # print error if not silent:
  shouldprint <- !silent && isTRUE(getOption("show.error.messages"))
  if(shouldprint || file!="") cat(if(file=="")paste0("tryStack error ", info),
                                  tryenv$emsg, file=file, append=TRUE, sep="")
  }
} # efun end


# warning/message function
wfun <- function(e) efun(e, type="warning")
mfun <- function(e) efun(e, type="message")

# now try the expression:
out <- try(withCallingHandlers(expr, error=efun, warning=wfun, message=mfun), silent=TRUE)
# add the trace stack character string to the output:
if(inherits(out, "try-error")) out[length(out)+1] <- tryenv$emsg
# Done! return the output:
return(invisible(out))
}

