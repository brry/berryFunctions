#' @title open file in default application
#' @description open a file using \code{\link{system2}} with command based on operating system.
#' Tries to open the file with the program associated with its file extension.
#' @return Result of try(system2, ...), invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @seealso \code{\link{system2}}, \code{\link{checkFile}}
#' @references \url{https://askubuntu.com/questions/15354},
#' \url{https://apple.stackexchange.com/questions/212583}
#' @keywords file
# @importFrom package fun1 fun2
#' @export
#' @examples
#' \dontrun{ # excluded from CRAN checks, file opening not wanted
#' openFile("README.md")
#' openFile("Tests.R")
#' openFile(c("README.md","Tests.R"))
#' is.error(openFile("dummydummydoesntexist.R"), TRUE, TRUE)
#' openFile(tempdir())
#' }
#' #' # To open folders with system2:
#' # "nautilus" on linux ubuntu
#' # "open" or "dolphin" on mac
#' # "explorer" or "start" on windows
#' 
#' @param file Filename to be opened, as character string.
#' @param \dots Further arguments passed to \code{\link{system2}}
#' 
openFile <- function(
file,
...
)
{
file <- normalizePath(file, winslash="/", mustWork=FALSE)
checkFile(file)
file <- shQuote(file) # to handle space in "C:/Program Files/R/..."
linux <- Sys.info()["sysname"]=="Linux"
out <- try(if(!linux) system2("open", file, ...) else  # Windows
                      system2("xdg-open", file, ...),  # Linux
           silent=TRUE)
return(invisible(out))
}
