#' get column from data.frame
#' 
#' (Try to) extract a column from a data frame with USEFUL warnings/errors.\cr
#' Watch out not to define objects with the same name as x if you are using
#' getColumn in a function!
#' 
#' @return Vector (or array, factor, etc) with values in the specified column
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2016
#' @seealso \code{\link{subset}}, \code{\link{getElement}}, \url{https://mran.revolutionanalytics.com/web/packages/car/vignettes/embedding.pdf}
#' @export
#' @examples
#' getColumn(Air.Flow, stackloss)
#' getColumn(2, stackloss)
#' getColumn("2",  stackloss) # works too...
#' getColumn(2,  stackloss[0,])
#' # The next examples all return errors:
#' try( getColumn(2,  stackloss[0])  )
#' try( getColumn(2,  stackloss[,0]) )
#' try( getColumn(Acid, stackloss)   ) # design choice: partial matching not supported
#' try( getColumn(2:3,  stackloss)   ) # cannot be a vector
#' try( getColumn(c("Air.Flow","Acid.Conc"),  stackloss)    )
#' 
#' upper <- function(x, select) getColumn(x, stackloss[select,])
#' upper(Water.Temp)
#' upper(2)
#' upper(2, select=0)
#' # upper(Water)  # error with useful message
#' 
#' # Pitfall lexical scoping: R only goes up until it finds things:
#' upper2 <- function(xx) {xx <- "Timmy!"; getColumn(xx, stackloss)} # will break!
#' is.error(      upper2(Water.Temp) ,   force=TRUE, tell=TRUE) # is an error
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
#' # Pitfall numerical column names:
#' df <- data.frame(1:5, 3:7)
#' colnames(df) <- c("a","1") # this is a bad idea anyways
#' getColumn("1", df) # will actually return the first column, not column "1"
#' 
#' getColumn(1, data.frame(AA=rep(NA,10)))
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
ndf <- if(substr(deparse(substitute(df)),1,10)=="substitute") as.character(df) else getName(df)
nam <- if(substr(deparse(substitute(x )),1,10)=="substitute") as.character(x ) else getName(x)
# deal with numeric input
namnum <- suppressWarnings(as.numeric(nam))
if(any(!is.na(namnum)))  nam <- colnames(df)[namnum]
# check if it's a data.frame
if(!all(class(df)=="data.frame"))
 {
 warning("df is not a data.frame, but a '", toString(class(df)), "'. Converting with as.data.frame.")
 df <- as.data.frame(df)
 }
# check if column exists:
if(length(colnames(df))==0) stop(calltrace, ndf, " has no columns.")
if(!nam %in% colnames(df)) stop(calltrace, "Column '", nam, "' is not in ", ndf,
                           ", which has the columns: ", toString(colnames(df)), ".")
# actually get the column:
out <- df[ , nam]
if(is.null(out) )
  {
  warning(calltrace, "Column '",nam,"' could not be extracted from ", ndf, ".")
  return(out)
  }
if(NROW(out)==0)
  {
  warning(calltrace, ndf, " has no rows.")
  return(out)
  }
# Further testing:
if(all(is.na(out)) ) warning(calltrace, "Column '", nam, "' in ", ndf, " only has NAs.")
# return column values
out
}
