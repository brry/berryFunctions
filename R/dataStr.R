#' str of datasets
#' 
#' Print the \code{\link{str}} of each dataset returned by \code{\link{data}}
#' 
#' @return invisible data.frame. If \code{msg=TRUE}, prints via \code{\link{message}} in a for loop.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, November 2015, in search of good datasets for teaching
#' @seealso \code{\link{str}}, \url{https://vincentarelbundock.github.io/Rdatasets/}
#' @keywords print documentation
#' @importFrom utils data str
#' @export
#' @examples
#' 
#' \dontrun{ ## View should not be used in examples
#' dataStr() # all loaded packages on search path (package=NULL)
#' # dataStr(package="datasets") # only datasets in base R package datasets
#' dataStr(only=TRUE) # sorted by nrow / ncol
#' d <- dataStr(only="data.frame") # data.frames only
#' sort(sapply(d$Object, function(dd) {sum(is.na(get(dd)))})) # datasets with NAs
#' head(d)
#' if(interactive()) View(d) # to sort in Rstudio Viewer
#' d[,c("Object","ncol","nrow")]
#' 
#' dataStr(heads=TRUE) # heads of all data.frames
#' 
#' # dataStr(package="hms") # no datasets in package
#' }
#' 
#' @param heads   Logical: display heads of all data.frames? 
#'                If TRUE, \code{only} is ignored. DEFAULT: FALSE
#' @param only    Charstring class: give information only about objects of that class.
#'                Can also be TRUE to sort output by nrow/ncol
#'                DEFAULT: NULL (ignore)
#' @param msg     Logical: message str info? DEFAULT: FALSE
#' @param package Package name. DEFAULT: NULL
#' @param view    Open dataframe with \code{\link{View}} (in Rstudio, if available)? DEFAULT: TRUE
#' @param \dots   Other arguments passed to \code{\link{data}}
#' 
dataStr <- function(
heads=FALSE,
only=NULL,
msg=heads,
package=NULL,
view=TRUE,
...
)
{
env <- new.env()
d <- data(..., package=package, envir=env)$results
d <- as.data.frame(d, stringsAsFactors=FALSE)
d <- d[d$Package!=".",] # From local objects
# change things like  "beaver1 (beavers)"  to  "beaver1"
if(nrow(d)==0) stop("No datasets found with 'package' set to ", 
                    toString(package),".")
itemsplit <- strsplit(d$Item, split=" ", fixed=TRUE)
d$Object <- sapply(itemsplit, "[", 1)
d$Call <- sapply(itemsplit, "[", 2)
d$Call <- gsub("(","",gsub(")","",d$Call, fixed=TRUE), fixed=TRUE)
d$Call[is.na(d$Call)] <- d$Object[is.na(d$Call)]
# sort alphabetically within packages:
d <- d[order(d$Package, tolower(d$Object)),]
if(heads)
return(invisible(sapply(d$Object, function(x){
 y <- get(x)
 if(!is.data.frame(y)) return(class(y))
 if(ncol(y)>5) y<- y[,1:5]
 out <- head(y)
 if(msg) cat("\n", x,"\n")
 if(msg) print(out)
 out})))
# remove columns
d$LibPath <- NULL 
d$Item <- NULL
# new columns
d$length <- NA
d$nrow <- NA
d$ncol <- NA
d$class <- NA
d$elements <- NA
for(i in 1:nrow(d))
  {
  x <- d[i,, drop=FALSE]
  data(list=x$Call, package=x$Package, envir=env)
  obj <- get(x$Object, envir=env) # getExportedValue(asNamespace(package), x$Object)
  d[i,"class"] <- toString(class(obj))
  d[i,"length"] <- if(inherits(obj, "data.frame")) NA else length(obj)
  d[i,"nrow"] <- replace(nrow(obj), is.null(nrow(obj)), NA)
  d[i,"ncol"] <- replace(ncol(obj), is.null(ncol(obj)), NA)
  if(is.data.frame(obj) | is.list(obj)) d[i,"elements"] <- toString(sapply(obj, class))
  doprint <- TRUE
  if(!is.null(only) && !isTRUE(only)) doprint <- inherits(obj, only)
  if(msg & doprint)
    {
    message(x$Package, "  |  ", x$Object, "  |  ", d[i,"class"], "  |  ", x$Title)
    message(str(obj))
    }
  }
d$Call <- NULL
if(!is.null(only)) 
  {
  d <- if(isTRUE(only)) d else d[grepl(only, d$class), ]
  d <- sortDF(d, "nrow")
  d <- sortDF(d, "ncol")
  }
if(view) # https://github.com/r-spatial/sf/issues/618
if(requireNamespace("rstudioapi", quietly=TRUE) && rstudioapi::isAvailable())
  .rs.viewHook(x=d, title="dataStr") else View(d, title="dataStr")
# Output:
message("see also   https://vincentarelbundock.github.io/Rdatasets/")
return(invisible(d))
}

# Suppress CRAN check note 'no visible binding for global variable':
if(getRversion() >= "2.15.1")  utils::globalVariables(".rs.viewHook")
