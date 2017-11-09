#' @title normalizePath Cross Platform
#' @description \code{\link{normalizePath}} Cross Platform: Returns absolute
#'              path even for not (yet) existing files even on Linux. 
#'              On Windows, this is the default behaviour.
#' @return path character string(s)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2017
#' @seealso \code{\link{normalizePath}}, \code{\link{newFilename}}
#' @keywords file
#' @export
#' @examples
#' 
#' normalizePath  ("doesnotexist.file", mustWork=FALSE) # on linux not full path
#' normalizePathCP("doesnotexist.file") # full path on all platforms
#' 
#' checknp <- function(a,b=a,d=getwd()) 
#'   {
#'   aa <- normalizePathCP(a)
#'   bb <- if(d=="") b else paste0(d,"/",b)
#'   if(aa != bb) stop("'", a, "' -> '", aa, "', should be '",bb, "'.")
#'   aa
#'   }
#' 
#' checknp("notexist.file")
#' checknp("../notexist.file", "notexist.file", dirname(getwd()))
#' checknp("notexistfolder/notexist.file")
#' #checknp("/home/berry/notexist.file", d="") # fails on windows
#' #checknp("S:/Dropbox/notexist.file",d="") # fails on linux
#'
#' @param path     Character vector of file paths
#' @param winslash Path separator on Windows. 
#'                 DEFAULT: "/" (unlike \code{\link{normalizePath}})
#' @param mustWork Logical for \code{\link{normalizePath}}. DEFAULT: FALSE
#'
normalizePathCP <- function(
path,
winslash="/",
mustWork=FALSE
)
{
# Windows + Mac work fine by default:
if(Sys.info()["sysname"] != "Linux") 
  return(normalizePath(path=path, winslash=winslash, mustWork=mustWork))
# Linux is more tricky
# fine if path exists
pathexist <- file.exists(path)
if(all(pathexist)) return(normalizePath(path=path, mustWork=mustWork))
# otherwise try going up the directory
dirup <- function(singlepath)
  {
  out <- singlepath # file.path(getwd(), singlepath)
  outend <- ""
  count <- 0
  while(!file.exists(out)) 
   {
   outend <- file.path(basename(out), outend)
   out <- normalizePath(dirname(out), mustWork=FALSE)
   # avoid endless looping (shouldn't happen, but just in case)
   count <- count+1
   if(count>20) stop("Cannot normalize path from '", singlepath,"'.")
   }
  # output
  
  file.path(out, sub("/$","",outend))
  } 
#
pathout <- path
pathout[pathexist] <- normalizePath(pathout[pathexist], mustWork=mustWork)
pathout[!pathexist] <- sapply(pathout[!pathexist], dirup)
pathout
}
