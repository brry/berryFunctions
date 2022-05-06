#' @title Run code in several R versions
#' @description Run code / script in several local R versions 
#' @return Results from each run
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2022
#' @seealso \code{\link{help}}
#' @export
#' @examples
#' tfile <- tempfile(fileext=".R")
#' cat(
#' 'trace <- function() paste(sapply(sys.calls(),function(x)
#'                      strsplit(deparse(x),"(", fixed=TRUE)[[1]][1]), collapse=" -> ")
#' lower <- function(a) {message(trace(), " - msg with ", a+10); a}
#' upper <- function(b) lower(b+5)
#' upper(3)', file=tfile)
#' 
#' # Don't actually run with example testing
#' # out <- source(tfile) ; out$value # message + output 8
#' # runRversions(tfile)
#' # runRversions(expr=5+7)
#'
#' @param scpt File path to script. DEFAULT: NULL
#' @param expr Expression to be run. DEFAULT: NULL
#' @param path Location of R versions. DEFAULT: "C:/Program Files/R/"
#' @param vrns R Versions at path. DEFAULT: dir(path,pattern="R-")
#' @param exec Local path to Rscript. DEFAULT: "/bin/Rscript.exe"
#'
runRversions <- function(
scpt=NULL,
expr=NULL,
path="C:/Program Files/R/",
vrns=dir(path, pattern="R-"),
exec="/bin/Rscript.exe"
)
{
fullexes <- paste0(path,vrns,exec)
expr_char <- deparse(substitute(expr)) 
if(!is.null(scpt)){
scpt <- normalizePathCP(scpt)
checkFile(scpt)
out <- pbapply::pblapply(fullexes, system2, scpt, stdout=TRUE)
}
if(!is.null(expr)){
#  system2(fullexes[1], paste("-e",shQuote(expr_char)))
out <- pbapply::pblapply(fullexes, system2, paste("-e",shQuote(expr_char)), stdout=TRUE)
}
names(out) <- vrns
out
}
