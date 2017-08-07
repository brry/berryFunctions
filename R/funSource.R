#' Source code of a function
#' 
#' open source code of a function in a loaded or specified package on github.com/cran or github.com/wch/r-source
#' 
#' @return links that are also opened with \code{\link{browseURL}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan+Dec 2016
#' @importFrom utils browseURL find
#' @export
#' @seealso \url{https://github.com/brry/rstudioberry} to add this as a keyboard shortcut
#' @examples
#' \dontrun{ ## browser windows should not be openend in CRAN checks
#' library("berryFunctions")
#' funSource(colPoints)
#' funSource("head")
#' funSource("require", trydirect=FALSE)
#' 
#' funSource(earthDist)
#' funSource(OSMscale::earthDist)
#' funSource("OSMscale::earthDist")
#' }
#' 
#' \dontrun{ # developmental testing
#' require(plotrix); require(scales)
#' funSource(rescale
#' 
#' tail <- function(...) stop("This is a dummy function. Type: rm(tail)")
#' funSource("tail")
#' rm(tail)
#' }
#' 
#' @param x function name, with or without quotation marks
#' @param character.only If TRUE, look for SomeFun instead of MyFun if
#'                       MyFun <- "SomeFun". DEFAULT: \code{\link{is.character}(x)}
#' @param trydirect If TRUE, try direct url to file \code{x.R}. DEFAULT: TRUE
#' 
funSource <- function(
x,
character.only=is.character(x),
trydirect=TRUE
)
{
# change input to character:
if (!character.only) x <- deparse(substitute(x))
if(length(x)>1) stop("length(x) must be 1, not ", length(x))

# Get package name -------------------------------------------------------------

# if package is specified: -----
if(grepl("::", x))
  {
  pn <- strsplit(x, "::")[[1]][1]
  x  <- strsplit(x, "::")[[1]][2]
  } else
{
# find locations of x: -----
locs <- find(x)
if(length(locs)<1) stop("A package containing '", x,"' was not found on the search path.")
if(locs[1]==".GlobalEnv")
  {
  warning(x," exists in .GlobalEnv, is ignored.")
  locs <- locs[-1]
  }
# get package name
pn <- strsplit(locs, ":")
pn <- sapply(pn, "[", 2)
if(all(is.na(pn))) stop("A package containing '", x,"' was not found on the search path.")
if(length(pn)>1)
  {
  warning("Using ", x, " in ", pn[1], ". It was also found in ", toString(pn[-1]))
  pn <- pn[1]
  }
}

# select mirror (base R or CRAN) -----------------------------------------------

if(pn %in% c("base", "compiler", "datasets", "grDevices", "graphics", "grid",
             "methods", "parallel", "profile", "splines", "stats", "stats4",
             "tcltk", "tools", "translations", "utils"))
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

# open link in Browser ---------------------------------------------------------

if(trydirect) browseURL(finallink)
# Search github repo query link
searchlink <- paste0("https://github.com/search?q=",x," function repo:",slink)
browseURL(searchlink)
# output
c(searchlink, finallink)
}

