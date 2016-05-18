#' check file existance
#'
#' check whether a file exists and give a useful error/warning/message
#'
#' @return TRUE/FALSE, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{file.exists}}
#' @keywords file
#' @export
#' @examples
#' is.error( fileCheck("FileThatDoesntExist.txt") )
#' fileCheck("FileThatDoesntExist.txt", fun=warning)
#' fileCheck("FileThatDoesntExist.txt", fun=message)
#' is.error( fileCheck("FileThatDoesntExist.txt", fun=MyWarn) ) # nonexisting function
#'
#' @param file Filename as character string to be checked for existence.
#' @param fun One of the functions \code{\link{stop}}, \code{\link{warning}}, or \code{\link{message}}. DEFAULT: stop
#'
fileCheck <- function(
file,
fun=stop
)
{
exi <- file.exists(file)
if(!exi) fun("  The file '", file, "'\n  does not exist at ", getwd() )
return(invisible(exi))
}
