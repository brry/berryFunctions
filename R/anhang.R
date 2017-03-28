#' open the Appendix of Rclick
#'
#' Open the Appendix of my R handbook found online at
#' \url{RclickHandbuch.wordpress.com} or directly at
#' \url{https://dl.dropbox.com/u/4836866/Rclick/Anhang.pdf}.
#'
#' @return None, opens pdf in default viewer using \code{\link{system2}} or \code{\link{browseURL}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2016
#' @seealso \code{\link{funSource}}
#' @export
#' @examples
#' # anhang() # excluded from cran check because of external browser opening policy
#'
anhang <- function()
{
file <- "S:/Dropbox/R/rclick/Anhang.pdf"
# laptop linux path change:
if(!file.exists(file)) file <- gsub("S:", "~", file)
# work PC path change:
if(!file.exists(file)) file <- gsub("~", "C:/Users/boessenkool", file)
# path control
# checkFile(file)
# open pdf
if(file.exists(file)) system2("open", file)
else
browseURL("https://www.dropbox.com/s/d72jnau7fzbqjnc/Anhang.pdf?dl=0")  
}
