#' str of datasets
#' 
#' Print the \code{\link{str}} of each dataset returned by \code{\link{data}}
#' 
#' @return invisible data.frame. Mainly prints via \code{\link{message}} in a for loop.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, November 2015, in search of good datasets for teaching
#' @seealso \code{\link{str}}
#' @keywords print documentation
#' @importFrom utils data str
#' @export
#' @examples
#' 
#' dataStr() # all loaded packages on search path (package=NULL)
#' # dataStr(package="datasets") # only datasets in base R package datasets
#' dataStr(TRUE) # sorted by nrow / ncol
#' 
#' d <- dataStr(only="data.frame") # data.frames only
#' head(d)
#' if(interactive()) View(d) # to sort in Rstudio Viewer
#' d[,c("Object","ncol","nrow")]
#' 
#' @param only    Charstring class: give information only about objects of that class.
#'                Can also be TRUE to sort output by nrow/ncol
#'                DEFAULT: NULL (ignore)
#' @param msg     Logical: message str info? DEFAULT: FALSE
#' @param package Package name. DEFAULT: NULL
#' @param \dots   Other arguments passed to \code{\link{data}}
#' 
dataStr <- function(
only=NULL,
msg=FALSE,
package=NULL,
...
)
{
env <- new.env()
d <- data(..., package=package, envir=env)$results
d <- as.data.frame(d, stringsAsFactors=FALSE)
# change things like  "beaver1 (beavers)"  to  "beaver1"
itemsplit <- strsplit(d$Item, split=" ", fixed=TRUE)
d$Object <- sapply(itemsplit, "[", 1)
d$Call <- sapply(itemsplit, "[", 2)
d$Call <- gsub("(","",gsub(")","",d$Call, fixed=TRUE), fixed=TRUE)
d$Call[is.na(d$Call)] <- d$Object[is.na(d$Call)]
# sort alphabetically within packages:
d <- d[order(d$Package, tolower(d$Object)),]
# remove columns
d$LibPath <- NULL 
d$Item <- NULL
d$Call <- NULL
# new columns
d$length <- NA
d$nrow <- NA
d$ncol <- NA
d$class <- NA
for(i in 1:nrow(d))
  {
  x <- d[i,, drop=FALSE]
  data(list=x$Call, package=x$Package, envir=env)
  obj <- get(x$Object, envir=env) # getExportedValue(asNamespace(package), x$Object)
  d[i,"class"] <- toString(class(obj))
  d[i,"length"] <- if(inherits(obj, "data.frame")) NA else length(obj)
  d[i,"nrow"] <- replace(nrow(obj), is.null(nrow(obj)), NA)
  d[i,"ncol"] <- replace(ncol(obj), is.null(ncol(obj)), NA)
  doprint <- TRUE
  if(!is.null(only) && !isTRUE(only)) doprint <- inherits(obj, only)
  if(msg & doprint)
    {
    message(x$Package, "  |  ", x$Object, "  |  ", d[i,"class"], "  |  ", x$Title)
    message(str(obj))
    }
  }
if(!is.null(only)) 
  {
  d <- if(isTRUE(only)) d else d[grepl(only, d$class), ]
  d <- sortDF(d, "nrow")
  d <- sortDF(d, "ncol")
  }
return(d)
}

# d <- dataStr(TRUE) ; rm(dataStr)
# d <- dataStr("data.frame")
