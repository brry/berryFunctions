#' Source code of a function
#' 
#' open source code of a function in a loaded package on github.com/cran or github.com/wch/r-source 
#' 
#' @return link that is also opened
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2016
#' @note This is not finished yet...
#' @importFrom utils browseURL find
#' @export
#' @examples
#' library("berryFunctions")
#' funSource(colPoints) 
#' funSource("head") 
#' funSource("require", trydirect=FALSE) 
#' 
#' @param x function name, with or without quotation marks
#' @param character.only If TRUE, look for SomeFun instead of MyFun if  
#'                       MyFun <- "SomeFun". DEFAULT: FALSE
#' @param trydirect If TRUE, try direct url to file \code{x.R}. DEFAULT: TRUE                      
#' 
funSource <- function(
x, 
character.only=FALSE,
trydirect=TRUE
)
{
# change input to character:
if (!character.only) x <- as.character(substitute(x))
# find locations of x:
locs <- find(x)
if(locs[1]==".GlobalEnv")
  {
  warning(x," exists in .GlobalEnv, is ignored.")
  locs <- locs[-1]
  }
# get package name
pn <- strsplit(locs, ":")
pn <- sapply(pn, "[", 2)
if(all(is.na(pn))) stop("A package containing '", x,"' was not found.")
if(length(pn)>1)
  {
  warning("Using ", x, " in ", pn[1], ". It was also found in ", toString(pn[-1]))
  pn <- pn[1]
  }
# select mirror (base R or CRAN)
if(pn %in% c("base", "compiler", "datasets", "grDevices", "graphics", "methods", 
        "parallel", "profile", "stats", "stats4", "tools", "translations", "utils"))
  {
  baselink <- "https://github.com/wch/r-source/tree/trunk/src/library/"
  finallink <- paste0(baselink,pn,"/R/", x,".R")
  slink <- paste0("wch/r-source+path:src/library/",pn,"/R")
  } else
  {
  baselink <- "https://github.com/cran/"
  finallink <- paste0(baselink,pn,"/blob/master/R/", x,".R")
  slink <- paste0("cran/",pn,"+path:R")
  }
# open link in Browser:
if(trydirect) browseURL(finallink)
# Search github repo query link
searchlink <- paste0("https://github.com/search?q=",x," function repo:",slink)
browseURL(searchlink)
# output
c(searchlink, finallink)
}



if(FALSE){

require(plotrix); require(scales)
funSource(rescale)

tail <- function(...) stop("This is a dummy function. Type: rm(tail)")
funSource("tail")
rm(tail)

}
