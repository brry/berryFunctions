#' open the Appendix of Rclick
#' 
#' Open the Appendix of my R handbook found online at
#' \url{https://github.com/brry/rclick}
#' 
#' @return None, opens pdf in default viewer using \code{\link{system2}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2016
#' @seealso \code{\link{funSource}}
#' @export
#' @examples
#' # anhang() # excluded from cran check because of external browser opening policy
#' 
anhang <- function()
{
file <- system.file("extdata/Anhang.pdf", package="berryFunctions")
openFile(file)
}
