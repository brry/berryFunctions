#' check file existance
#' 
#' check whether files exist and give a useful error/warning/message
#' 
#' @return TRUE/FALSE, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{file.exists}}
#' @keywords file
#' @export
#' @examples
#' is.error( checkFile("FileThatDoesntExist.txt")  )
#' checkFile("FileThatDoesntExist.txt", warnonly=TRUE)
#' checkFile("FileThatDoesntExist.txt", warnonly=TRUE, trace=FALSE)
#' 
#' \dontrun{## Excluded from CRAN checks because of file creation
#' # Vectorized:
#' file.create("DummyFile2.txt")
#' checkFile(paste0("DummyFile",1:3,".txt"), warnonly=TRUE)
#' checkFile(paste0("DummyFile",1:3,".txt") )
#' file.remove("DummyFile2.txt")
#' 
#' compareFiles("dummy.nonexist", "dummy2.nonexist")
#' checkFile("dummy.nonexist")
#' }
#' 
#' dingo <- function(k="brute.nonexist", trace=TRUE)
#'          checkFile(k, warnonly=TRUE, trace=trace)
#' dingo()
#' dingo("dummy.nonexist")
#' 
#' upper <- function(h, ...) dingo(c(h, "dumbo.nonexist"), ...)
#' upper("dumbo2.nonexist")
#' upper(paste0("dumbo",2:8,".nonexist"))
#' upper(paste0("dumbo",2:8,".nonexist"), trace=FALSE)
#' 
#' 
#' @param file Filename(s) as character string to be checked for existence.
#' @param warnonly Logical: Only issue a \code{\link{warning}} instead of an
#'                 error with \code{\link{stop}}? DEFAULT: FALSE
#' @param trace Logical: Add function call stack to the message? DEFAULT: TRUE
#' 
checkFile <- function(
file,
warnonly=FALSE,
trace=TRUE
)
{
# check actual file existence:
exi <- file.exists(file)
# warn or stop if file nonexistent:
if(any(!exi))
  {
  # tracing the calling function(s):
  Text1 <- if(trace) traceCall(prefix="in ", suffix=" :  ") else ""
  # prepare message:
  Text2 <- if(sum(!exi)>1) paste0("The ",sum(!exi)," files  ") else "The file '"
  Text3 <- if(sum(!exi)>2) paste0(toString(file[!exi][1:2]), " (and ",sum(!exi)-2," others)") else
                           toString(file[!exi])
  Text4 <- if(sum(!exi)>1) "\n  do" else "'\n  does"
  Text5 <- paste0(" not exist at ", getwd() )
  Text <- paste0(Text1,Text2,Text3,Text4,Text5)
  if(warnonly) warning(Text, call.=!trace) else stop(Text, call.=!trace)
  }
return(invisible(exi))
}
