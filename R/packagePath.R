#' @title Base path of package
#' @description Base path of package (with DESCRIPTION file), per default at current getwd. 
#'              Derived from devtools::package_file 
#' @return Path character string
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2017
#' @seealso \code{\link{getwd}}
#' @keywords file
#' @export
#' @examples
#' # packagePath() # may fail on cran checks
#'
#' @param path Path to (or below) package directory. DEFAULT: "."
#'
packagePath <- function(
path="."
)
{
path <- path0 <- normalizePath(path, winslash="/", mustWork=FALSE)
checkFile(path, pwd=FALSE)
# Go up the path until DESCRIPTION exists
while(!file.exists(file.path(path, "DESCRIPTION")))
  {
  path <- dirname(path)
  # Error if path0 was not below or at a package:
  if(path==dirname(path)) 
    stop(traceCall(skip=1, prefix="in ", suffix=":\n"), "Package root directory ",
         "(containing DESCRIPTION file) could not be found for ", path0, call.=FALSE)
  }
path
}