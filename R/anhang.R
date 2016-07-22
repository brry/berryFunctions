#' open the Appendix of Rclick
#'
#' Open the Appendix of my R handbook found online at
#' \url{RclickHandbuch.wordpress.com} or directly at
#' \url{https://dl.dropboxusercontent.com/u/4836866/Rclick/Anhang.pdf}.
#' This function is rather for internal usage on my two computers.
#'
#' @return None, opens pdf in default viewer using \code{\link{system2}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2016
#' @seealso \code{\link{funSource}}
#' @export
#' @examples
#'
anhang <- function()
{
file <- "S:/Dropbox/Public/Rclick/Anhang.pdf"
# laptop linux path change:
if(!file.exists(file)) file <- gsub("S:", "~", file)
# work PC path change:
if(!file.exists(file)) file <- gsub("~", "C:/Users/boessenkool", file)
# path control
checkFile(file)
# open pdf
system2("open", file)
}
