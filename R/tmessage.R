#' @title messages with call trace
#' @description [message], [warning] or [stop] with a call trace prepended
#' @md
#' @param skip Number of tracing levels to exclude. DEfault: 0
#' @param ... Passed to [message], [warning] or [stop]
#' @aliases tmessage twarning tstop
#' @return NULL, as per [message], [warning] or [stop]
#' @export
#' @seealso [traceCall] for the generation of the trace
#' @examples
#' lower <- function(a, s) {tmessage(s, "some stuff with ", a+10); a}
#' upper <- function(b, skip=0) lower(b+5, skip)
#' upper(3) # upper -> lower: some stuff with 18
#' upper(3, skip=1) # no "lower" in trace
#' upper(3, skip=-1) # upper -> lower -> tmessage: some stuff with 18

tmessage <- function(skip=0, ...) {x <- traceCall(skip=skip+1, prefix="", suffix=": "); message(x, ...)}
#' @rdname tmessage
#' @export
twarning <- function(skip=0, ...) {x <- traceCall(skip=skip+1, prefix="", suffix=": "); warning(x, ...)}
#' @rdname tmessage
#' @export
tstop    <- function(skip=0, ...) {x <- traceCall(skip=skip+1, prefix="", suffix=": ");    stop(x, ...)}
