#' get column from data.frame
#'
#' Extract columns if they are given in a data frame. 
#' Watch out not to define objects with the same name as x if you are using
#' getColumn in a function!
#'
#' @return Vector (or array, factor, etc) with values in the specified column
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2016
#' @seealso \code{\link{subset}}, \url{https://mran.revolutionanalytics.com/web/packages/car/vignettes/embedding.pdf}
#' @export
#' @examples
#' getColumn(Air.Flow, stackloss)
#' is.error(getColumn(Acid, stackloss))
#' 
#' upper <- function(x) getColumn(x, stackloss)
#' upper(Water.Temp)
#' # upper(Water) # error (design choice: partial matching not supported)
#' 
#' upper2 <- function(xx) {xx <- 17; getColumn(xx, stackloss)}
#' stopifnot(is.error(      upper2(Water.Temp)       )) # breaks
#' 
#' upper3 <- function(xx, dd) getColumn(substitute(xx), dd)
#' upper3(Air.Flow, stackloss) # may be safer in many scoping situations
#' 
#' # In packages use "colname" with quotation marks in level 2 functions to avoid 
#' # the CRAN check NOTE "no visible binding for global variable"
#' 
#' df <- data.frame(x=letters[1:3],y=letters[4:6]) 
#' is.vector(df$x)
#' is.vector(getColumn("x", df)) # FALSE
#' # cannot force output to be a vector, as this will convert:
#' as.Date("2016-09-14")  ;  as.vector(as.Date("2016-09-14"))
#' # same problem with dfs from tapply results
#' # better ideas welcome!! (berry-b@gmx.de)
#'
#' @param x Column name to be subsetted. The safest is to use character strings
#'          or \code{\link{substitute}(input)}.
#'          If there is an object "x" in a function environment, 
#'          its value will be used as name! (see upper2 example)
#' @param df dataframe object
#' @param trace Logical: Add function call stack to the message? DEFAULT: TRUE
#'              WARNING: in do.call settings with large objects,
#'              tracing may take a lot of computing time.
#'
getColumn <- function(
x,
df,
trace=TRUE
)
{
calltrace <- if(trace) traceCall() else ""
# get names of objects as character strings:
nam <- if(substr(deparse(substitute(x )),1,10)=="substitute") as.character(x ) else getName(x)
ndf <- if(substr(deparse(substitute(df)),1,10)=="substitute") as.character(df) else getName(df)
# check if column exists:
if(!nam %in% colnames(df)) stop(calltrace, "'", nam, "' is not in ", ndf,
                           ", which has the columns: ", toString(colnames(df)), ".")
# actually get the column:
out <- df[ , nam]
if(is.null(out) ) stop(calltrace, "'", nam,
                       "' could not be extracted from ", ndf, ".")
if(all(is.na(out)) ) warning(calltrace, "'", nam, "' in ", ndf, " only has NAs.")
# return column values
out
}
