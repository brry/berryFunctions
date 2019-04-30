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
#' # data.frames only
#' d <- dataStr(df=TRUE)
#' head(d)
#' d[,c("Call","ncol","nrow")]
#' 
#' @param package Package name. DEFAULT: NULL
#' @param df      Logical: give information only about all data.frame objects? 
#'                DEFAULT: FALSE
#' @param \dots other arguments passed to \code{\link{data}}
#' 
dataStr <- function(
package=NULL,
df=FALSE,
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
if(df)
  {
  d$nrow <- NA
  d$ncol <- NA
  }
for(i in 1:nrow(d))
  {
  x <- d[i,, drop=FALSE]
  data(list=x$Call, package=x$Package, envir=env)
  obj <- get(x$Object, envir=env) # getExportedValue(asNamespace(package), x$Object)
  d[i,"class"] <- toString(class(obj))
  if(!df)
    {
    message(x$Package, "  |  ", x$Object, "  |  ", d[i,"class"], "  |  ", x$Title)
    message(str(obj))
    } else if(grepl("data.frame", d[i,"class"]))
    d[i, c("nrow","ncol")] <- c(nrow(obj),ncol(obj))
  }
if(df) 
  {
  d <- d[grepl("data.frame", d$class), ]
  d <- sortDF(d, "nrow")
  d <- sortDF(d, "ncol")
  }
return(invisible(d))
}


