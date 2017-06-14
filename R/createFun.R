#' create function framework
#'
#' create a file with a complete (Roxygen) framework for a new function in a package
#'
#' @details Tries to open the file in the standard editor for .R files using \code{\link{system2}}
#'
#' @return file name as character string
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, March 2016
#' @seealso \code{\link{system2}}, \code{\link{funSource}}, Roxygen2: 
#'          \url{https://cran.r-project.org/package=roxygen2/vignettes/rd.html}
#' @keywords documentation
#' @export
#' @examples

#' #createFun("myNewFunction")

#' @param fun  Character string or unquoted name. Function that will be created with identical filename.
#' @param path Path to package in development (including package name itself).
#'             Paths ending in /R,man,inst,vignettes will be changed to one level up.
#'             DEFAULT: "."
#'
createFun <- function(
fun,
path="."
)
{
# check and deparse input:
fun <- deparse(substitute(fun))
fun <- gsub("\"", "", fun, fixed=TRUE)
if(length(fun) >1)     stop("'fun' must be a single function name.")
if(length(path)>1)     stop("'path' must be a single character string.")
checkFile(path)
# Filename
path <- normalizePath(path, winslash="/", mustWork=FALSE)
if(endsWith(path, "/R"))         path <- substr(path, 1, nchar(path)-2)
if(endsWith(path, "/man"))       path <- substr(path, 1, nchar(path)-4)
if(endsWith(path, "/inst"))      path <- substr(path, 1, nchar(path)-5)
if(endsWith(path, "/vignettes")) path <- substr(path, 1, nchar(path)-10)
rfile <- paste0(path,"/R/",fun,".R")
rfile <- newFilename(rfile) # append _1 if existent
#
# Date
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
date <- paste0(format(Sys.Date(), "%b %Y"), "\n")
Sys.setlocale("LC_TIME", lct)
#
# Write function structure
part1 <- paste0(
"' @title title
' @description description
' @details detailsMayBeRemoved
' @aliases aliasMayBeRemoved
' @return ReturnValue
' @section Warning: warningMayBeRemoved
' @author Berry Boessenkool, \\email{berry-b@@gmx.de}, ", date, 
"' @seealso \\code{\\link{help}}, \\code{\\link{help}}
' @keywords aplot
 @importFrom package fun1 fun2
' @export
' @examples
'
'
' @param
' @param
' @param
' @param \\dots Further arguments passed to \\code{\\link{plot}}
'
")
part2 <- paste0("\n",
fun," <- function(

)
{

}
")

part1 <- paste0("#", strsplit(part1, "\n", fixed=TRUE)[[1]])
part1 <- paste(part1, collapse="\n")
cat(part1,part2, file=rfile, sep="")
# Open the file with the program associated with its file extension:
linux <- Sys.info()["sysname"]=="Linux"
try(if(!linux) system2("open", rfile) else system2("xdg-open", rfile), silent=TRUE)
# return file name:
invisible(rfile)
}

