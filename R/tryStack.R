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
#' d <- tryStack(upper("42"), silent=TRUE) # like try, but with traceback, even for warnings
#' cat(d)
#' d <- tryStack(upper("42"), silent=TRUE, warn=0) # don't touch warnings
#' 
#' tryStack(upper(42)) # returns normal output, but warnings are easier to debug
#' 
#' stopifnot(inherits(d, "try-error"))
#' stopifnot(tryStack(upper(42))==52)
#' 
#' \dontrun{ ## file writing not wanted by CRAN checks
#' d <- tryStack(upper("42"), silent=TRUE, file="log.txt")
#' system2("open", "log.txt")
#' unlink("log.txt")
#' }
#' 
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
#' @param silent   Logical: Should error message + stack printing be suppressed?
#'                 DEFAULT: FALSE
#' @param warn     Integer. 0: Don't touch \code{\link{warning}s} and \code{\link{message}s}
#'                 1: warning in upper -> middle -> lower: message.
#'                 2: complete calling stack as with errors.
#'                 If 1 or 2, they are still handled like regular warnings / messages.
#'                 (unless \code{file !=""}). DEFAULT: 1
#' @param file     File name passed to \code{\link{cat}}. 
#'                 If given, Errors will be appended to the file after two empty lines. 
#'                 if \code{warn>0} and file!="", warnings will not be shown, 
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
warn=1,
file="",
removetry=TRUE
)
{
# silence warnings:
if(warn>0)
  {
  oop <- options(warn=-1)
  on.exit(options(oop))
  }
if(!warn %in% 0:2) stop("warn must be 0, 1, or 2, not ", warn)
if(is.na(file)) file <- ""
# environment for stack to (potentially) be written into:
tryenv <- new.env()
assign("emsg", value="-- empty error stack --", envir=tryenv)
assign("wmsg", value=c("-- empty warning stack --", "-- warning stack --"), envir=tryenv)

# strings that will be removed from stack if matching exactly:
toremovestring <- "try(withCallingHandlers(expr, error = efun, warning = wfun, message = mfun),"
if(removetry) toremovestring <- c(toremovestring,
                     "tryCatch(expr, error = function(e) {",
                     "tryCatchList(expr, classes, parentenv, handlers)",
                     "tryCatchOne(expr, names, parentenv, handlers[[1L]])",
                     "doTryCatch(return(expr), name, parentenv, handler)",
                     ".handleSimpleError(function (e)",
                     "h(simpleError(msg, call))" 
                     )

# error function
efun <- function(e)
  {
  # stack of calls, in case of an error:
  stack <- sys.calls()
  # language to character:
  stack <- sapply(stack, deparse)
  # remove elements from tryStack:
  toremove <- sapply(stack, function(x) any(grepl(removeSpace(x[1]), toremovestring, fixed=TRUE)) )
  stack <- stack[!toremove]
  toremove2 <- sapply(stack, function(x) substr(x[1],1,9)=="tryCatch(") # this one may remove too much
  if(removetry) stack <- stack[!toremove2]
  # combine vectors into a single string:
  stack <- lapply(stack, function(x) paste(x, collapse="\n"))
  # add error code:
  ccall <- deparse(conditionCall(e))[1L]
  stack <- c(stack, ccall)
  # add numbers:
  stack <- sapply(seq_along(stack), function(i) paste0(i, ": ", stack[[i]]))
  # add descriptor:
  cmes <- conditionMessage(e)
  stack <- c(if(file!="") paste0("Error in ", ccall,": ", cmes),
             "tryStack sys.calls() error stack: ", 
             paste0("m: ", cmes), 
             rev(stack))
  # add empty lines (-> line breaks -> readability), if file is given:
  if(file!="") stack <- c("---------------",as.character(Sys.time()),"",stack,"","")
  # put message into main function environment:
  assign(x="emsg", value=paste(stack,collapse="\n"), envir=tryenv)
  # print if not silent:
  shouldprint <- !silent && isTRUE(getOption("show.error.messages"))
  if(shouldprint || file!="") cat(tryenv$emsg, file=file, append=TRUE)
  }

# warning function
wfun <- function(e, mes=FALSE)
  {
  if(warn==0) return()
  meswarn <- if(mes) "message " else "warning "
  # stack of calls, in case of a warning:
  stack <- sys.calls()
  # remove the warning part:
  stack <- head(stack, -7)
  # language to character:
  stack <- sapply(stack, deparse)
  # remove recursive warning part:
  irecwarn <- grep(".signalSimpleWarning(", stack, fixed=TRUE)
  if(length(irecwarn)>0) if(stack[irecwarn+2] == "withOneRestart(expr, restarts[[1L]])")
      stack <- stack[-(irecwarn+0:2)]
  stack <- stack[stack!="doWithOneRestart(return(expr), restart)"]
  # remove elements from tryStack:
  toremove <- sapply(stack, function(x) any(grepl(removeSpace(x[1]), toremovestring, fixed=TRUE)) )
  stack <- stack[!toremove]
  toremove2 <- sapply(stack, function(x) substr(x[1],1,9)=="tryCatch(") # this one may remove too much
  if(removetry) stack <- stack[!toremove2]
  # combine vectors into a single string:
  stack <- lapply(stack, function(x) paste(x, collapse="\n"))
  
  # only function names (warn=1)
if(warn==1)
{
  # shorten do.call (function( LONG ( STUFF)))
  stack <- lapply(stack, function(x) if(substr(x,1,7)=="do.call") 
               sub(",", "(", sub("(", " - ", x, fixed=TRUE), fixed=TRUE) else x)
  # keep try calls more informative:
  stack <- lapply(stack, function(x) if(substr(x,1,4)=="try(") 
               sub("(", " - ", x, fixed=TRUE) else x)  
  stack <- sapply(strsplit(unlist(stack), "(", fixed=TRUE), "[", 1)
}
  # add warning code:
  ccall <- deparse(conditionCall(e))[1L]
  stack <- c(stack, ccall)
  # add numbers:
  if(warn==2) stack <- sapply(seq_along(stack), function(i) paste0(i, ": ", stack[[i]]))
  # add descriptor:
  cmes <- conditionMessage(e)
  # catch nested (recursive) calling:
  wmes <- paste0("tryStack sys.calls() ", meswarn, "stack: ")
  cmes <- strsplit(cmes, paste0("\n",wmes), fixed=TRUE)[[1]][1]
if(warn==2)
{
  info <- c(paste0(if(file!="") meswarn, "in ", ccall, ": ", cmes), wmes)
  stack <- c(info, paste0("m: ", cmes), rev(stack))
}
if(warn==1)
{
  stack <- paste0(if(file!="") meswarn, "in ", paste(stack,collapse=" -> "), ": ", cmes)
}
  # add empty lines (-> line breaks -> readability), if file is given:
  if(file!="") stack <- c("---------------",as.character(Sys.time()),"",stack,"","")
  # create single character string (potentially with newlines):
  stack <-  paste(stack, collapse="\n")
  # put message into main function environment:
  assign(x="wmsg", value=stack, envir=tryenv)
  # warn:
  if(file!="")            cat(tryenv$wmsg, file=file, append=TRUE)
  if(file=="" & !mes) warning(tryenv$wmsg, immediate.=TRUE, call.=FALSE)
  if(file=="" & mes)  message("Message: ", tryenv$wmsg, appendLF=FALSE)    
  if(mes) invokeRestart("muffleMessage")
  }

# message function
mfun <- function(e) wfun(e, mes=TRUE)

# now try the expression:
out <- try(withCallingHandlers(expr, error=efun, warning=wfun, message=mfun), silent=silent)
# add the trace stack character string to the output:
if(inherits(out, "try-error")) out[length(out)+1] <- tryenv$emsg
# Done! return the output:
out
}

