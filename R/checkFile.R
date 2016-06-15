#' check file existance
#'
#' check whether a file exists and give a useful error/warning/message
#'
#' @return TRUE/FALSE, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{file.exists}}
#' @keywords file
#' @importFrom utils capture.output
#' @export
#' @examples
#' is.error( checkFile("FileThatDoesntExist.txt") )
#' checkFile("FileThatDoesntExist.txt", fun=warning)
#' checkFile("FileThatDoesntExist.txt", fun=message)
#' is.error( checkFile("FileThatDoesntExist.txt", fun=MyWarn) ) # nonexisting function
#' 
#' \dontrun{## Excluded from CRAN checks because of file creation
#' # Vectorized:
#' file.create("DummyFile2.txt")
#' checkFile(paste0("DummyFile",1:3,".txt"), fun=message)
#' checkFile(paste0("DummyFile",1:3,".txt") ) 
#' file.remove("DummyFile2.txt")
#' }
#' 
#' \dontrun{## Excluded from CRAN checks because of intentional errors
#' compareFiles("dummy.nonexist", "dummy2.nonexist")
#' checkFile("dummy.nonexist")
#'
#' dingo <- function(k="brute.nonexist") checkFile(k)
#' dingo()
#' dingo("dummy.nonexist")
#'
#' upper <- function(h) dingo(c(h, "dumbo.nonexist"))
#' upper("dumbo2.nonexist")
#' }
#'
#' @param file Filename(s) as character string to be checked for existence.
#' @param fun One of the functions \code{\link{stop}}, \code{\link{warning}}, or \code{\link{message}}. DEFAULT: stop
#' @param trace Logical: Add function call stack to the message? DEFAULT: TRUE
#' @param \dots Further arguments passed to \code{fun}
#'
checkFile <- function(
file,
fun=stop,
trace=TRUE,
...
)
{
# tracing the calling function(s):
if(trace)
  {
  dummy <- capture.output(tb <- traceback(6) )
  calltrace <- sapply(strsplit(unlist(tb), "(", fixed=TRUE), "[", 1)
  calltrace <- paste(rev(calltrace), collapse=" -> ")
  }
# check actual file existence:
exi <- file.exists(file)
# prepare message:
Text <- if(length(exi)>1)
paste0("  The files '", toString(file[!exi]), "'\n  do not exist at ", getwd()) else
paste0("  The file '",           file,      "'\n  does not exist at ", getwd())
#
if(trace) Text <- paste(calltrace, Text, sep="\n")
# return message, if file nonexistent:
if(any(!exi)) fun(Text, ...)
return(invisible(exi))
}
