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
#' # dataStr() # all loaded packages on search path (package=NULL)
#' dataStr("datasets") # only datasets in base R
#' dataStr("colorspace") # works with an installed but unloaded package
#' 
#' 
#' # Selection of dataframes based on columns ----
#' d <- dataStr()
#' d <- d[grepl("data.frame", d$class), ]
#' d$nrow <- NA
#' d$ncol <- NA
#' env <- new.env()
#' for(i in 1:nrow(d))
#'   {
#'   x <- d[i,, drop=FALSE]
#'   data(list=x$Call, package=x$Package, envir=env)
#'   obj <- get(x$Object, envir=env) # getExportedValue(asNamespace(package), x$Object)
#'   d[i, c("nrow","ncol")] <- c(nrow(obj),ncol(obj))
#'   }
#' d <- sortDF(d, ncol)
#' d[,c("Call","ncol","nrow")]
#' 
#' @param package Package name. DEFAULT: NULL
#' @param \dots other arguments passed to \code{\link{data}}
#' 
dataStr <- function(
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
d$class <- NA
for(i in 1:nrow(d))
  {
  x <- d[i,, drop=FALSE]
  data(list=x$Call, package=x$Package, envir=env)
  obj <- get(x$Object, envir=env) # getExportedValue(asNamespace(package), x$Object)
  message(x$Package, "  |  ", x$Object, "  |  ", toString(class(obj)), "  |  ", x$Title)
  message(str(obj))
  d[i,"class"] <- toString(class(obj))
  }
return(invisible(d))
}


