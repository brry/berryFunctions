#' @title open file in default application
#' @description open a file using \code{\link{system2}} with command based on operating system.
#' Tries to open the file with the program associated with its file extension.\cr
#' See \code{\link{openPDF}} to open files with sumatraPDF.
#' @return Result of try(system2, ...), invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @seealso \code{\link{openPDF}}, \code{\link{system2}}, \code{\link{checkFile}}
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
#' #' # To open folders (not files) with system2:
#' # "nautilus" on linux ubuntu
#' # "open" or "dolphin" on mac
#' # "explorer" or "start" on windows
#' # But open / xdg-open seems to work as well
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
sysname <- Sys.info()["sysname"]
out <- try(switch(sysname,
                  "Linux"   = system2("xdg-open", file, ...),
                  "FreeBSD" = system2("handlr", paste("open",file), ...),
                  "Windows" = for(fn in file) system2("cmd", args=c("/c","start","/B",'""',fn), wait=FALSE),
                  system2("open", file, ...) # on Windows, this closes sumatra when switching Rstudio projects
                  # system2("powershell", args=c("-Command", paste0("Start-Process '",file,"'")), wait=FALSE)
                  ),
           silent=TRUE)
# out: 127 if failed, 124 for timeout, 0 for success

return(invisible(out))
}
