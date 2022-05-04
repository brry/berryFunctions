#' @title messages with call trace
#' @description [message], [warning] or [stop] with a call trace prepended
#' @md
#' @param ...       Passed to [message], [warning] or [stop]
#' @param skip      Number of tracing levels to exclude. Default: 0
#' @param call.     include twarning/tstop call? DEFAULT: FALSE (unlike the originals)
#' @param noBreaks. reduce line breaks if `options(warn=1)`? 
#'                  DEFAULT: TRUE (unlike the original)
#' @aliases tmessage twarning tstop
#' @return NULL, as per [message], [warning] or [stop]
#' @export
#' @seealso [traceCall] for the generation of the trace
#' @examples
#' lower <- function(a, s) {tmessage("some stuff with ", a+10, skip=s); a}
#' upper <- function(b, skip=0) lower(b+5, skip)
#' upper(3) # upper -> lower: some stuff with 18
#' upper(3, skip=1) # no "lower" in trace
#' upper(3, skip=-1) # upper -> lower -> tmessage: some stuff with 18
#' tmessage("Some message", " to be displayed")
#' lower <- function(a, s) {twarning("some stuff with ", a+10, skip=s); a}
#' upper(7)
#' oop <- options(warn=1)
#' upper(7) # Warning: upper -> lower: some []       no line break :)
#' options(oop) ; rm(oop)
#' lower <- function(a, s) {tstop("some stuff with ", a+10, skip=s); a}
#' try(  upper(7)  ) # Error : try -> upper -> lower: some stuff with 22

tmessage <- function(..., skip=0) 
  {
  x <- traceCall(skip=skip+1, prefix="", suffix=": ")
  message(x, ...)
  }

#' @rdname tmessage
#' @export
twarning <- function(..., skip=0, call.=FALSE, noBreaks.=TRUE) 
  {
  x <- traceCall(skip=skip+1, prefix="", suffix=": ")
  warning(x, ..., call.=call., noBreaks.=noBreaks.)
  }

#' @rdname tmessage
#' @export
tstop <- function(..., skip=0, call.=FALSE) 
  {
  x <- traceCall(skip=skip+1, prefix="", suffix=": ")
  stop(x, ..., call.=call.)
  }
