#' install.package and require
#' 
#' install and load a package. If a package is not available, it is installed before being loaded
#' 
#' @aliases library2 require2
#' @return \code{\link{message}s} help instruction.
#' @note Passing a vector with packages will work, but give some warnings.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2014
#' @seealso \code{\link{install.packages}}, \code{\link{library}}
#' @keywords package
#' @export
#' @examples
#' 
#' \dontrun{
#' ## Excluded fom CRAN checks. Package installation on server is unnecessary.
#' require2(ada)
#' library2("statmod")
#' }
#' 
#' @param name Name of the package(s). Can be qouted, must not.
#' @param libargs List of arguments passed to \code{\link{library}} like \code{lib.loc}, \code{quietly} etc. DEFAULT: NULL
#' @param \dots Arguments passed to \code{\link{install.packages}} like \code{lib}, \code{repos} etc.
#' 
library2 <- function(
name,
libargs=NULL,
...)
{
name <- as.character(substitute(name))
for(n in name)
{
if(!requireNamespace(n, quietly=TRUE))  install.packages(n, ...)
do.call(library, owa(list(package=n, character.only=TRUE), libargs, "package", "character.only"))
}
for(n in name)
  message(paste0('-------------------------\nhelp(package="', n,
            '")\n-------------------------\n'))
}

#' @export
require2 <- library2
