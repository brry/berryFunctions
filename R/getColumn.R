#' get column from data.frame
#' 
#' (Try to) extract a column from a data frame with USEFUL warnings/errors.\cr
#' Watch out not to define objects with the same name as x if you are using
#' getColumn in a function!
#' 
#' @return Vector with values in the specified column
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2016
#' @seealso \code{\link{subset}}, \code{\link{getElement}}, \url{https://mran.revolutionanalytics.com/web/packages/car/vignettes/embedding.pdf}
#' @export
#' @examples
#' head(stackloss)
#' getColumn(Air.Flow, stackloss)
#' getColumn("Air.Flow", stackloss)
#' getColumn(2, stackloss)
#' getColumn("2",  stackloss) # works too...
#' 
#' # useful warnings:
#' getColumn(1, stackloss[0,])
#' getColumn(1, data.frame(AA=rep(NA,10)) )
#' 
#' # Code returning a character works as well:
#' getColumn(c("Air.Flow","Acid.Conc")[1],  stackloss)
#' 
#' # Can be used in functions to get useful messages:
#' upper <- function(x, select) getColumn(x, stackloss[select,])
#' upper(Water.Temp)
#' upper(2)
#' upper(2, select=0)
#' 
#' checkerr <- function(x) invisible(is.error(x, force=TRUE, tell=TRUE))
#' 
#' # Pitfall lexical scoping: R only goes up until it finds things:
#' upper2 <- function(xx) {xx <- "Timmy!"; getColumn(xx, stackloss)} # breaks!
#' checkerr( upper2(Water.Temp) ) # Column "Timmy" does not exist
#' # If possible, use "colname" with quotation marks.
#' # This also avoids the CRAN check NOTE "no visible binding for global variable"
#' upper3 <- function(char=TRUE)
#' {
#' Sepal.Length <- iris
#' if(char) colPoints("Sepal.Length", Sepal.Width, Petal.Length, data=iris, add=FALSE)
#' else     colPoints( Sepal.Length,  Sepal.Width, Petal.Length, data=iris, add=FALSE)
#' }
#' checkerr( upper3(char=FALSE) )
#' upper3(char=TRUE) # use string "Sepal.Length" and it works fine. 
#' 
#' 
#' # The next examples all return informative errors:
#' checkerr( upper(Water) ) #  partial matching not supported by design
#' checkerr( getColumn("dummy", stackloss)) # no NULL for nonexisting columns
#' checkerr( getColumn(2,  stackloss[,0]) ) # error for empty dfs
#' checkerr( getColumn(Acid, stackloss)   ) # no error-prone partial matching
#' checkerr( getColumn(2:3,  stackloss)   ) # cannot be a vector
#' checkerr( getColumn(c("Air.Flow","Acid.Conc"),  stackloss) )
#' 
#' 
#' #getColumn("a", tibble::tibble(a=1:7, b=7:1)) # works but warns with tibbles
#'  
#' # Pitfall numerical column names:
#' df <- data.frame(1:5, 3:7)
#' colnames(df) <- c("a","1") # this is a bad idea anyways
#' getColumn("1", df) # will actually return the first column, not column "1"
#' getColumn("1", df, convnum=FALSE)  # now gives second column
#' # as said, don't name column 2 as "1" - that will confuse people
#' 
#' # More on scoping and code yielding a column selection:
#' upp1 <- function(coln, datf) {getColumn(substitute(coln), datf)[1:5]}
#' upp2 <- function(coln, datf) {getColumn(           coln,  datf)[1:5]}
#' upp1(Sepal.Length, iris)
#' upp2(Sepal.Length, iris)
#' upp1("Sepal.Length", iris)
#' upp2("Sepal.Length", iris)
#' vekt <- c("Sepal.Length","Dummy")
#' upp1(vekt[1], iris)
#' upp2(vekt[1], iris)
#' 
#' @param x Column name to be subsetted. The safest is to use character strings
#'          or \code{\link{substitute}(input)}.
#'          If there is an object "x" in a function environment,
#'          its value will be used as name! (see upper2 example)
#' @param df dataframe object
#' @param trace Logical: Add function call stack to the message? DEFAULT: TRUE
#' @param convnum Logical: Convert numerical input (even if character) to 
#'                Column name for that number?
#' 
getColumn <- function(
x,
df,
trace=TRUE,
convnum=TRUE
)
{
calltrace <- if(trace) traceCall(prefix="in ", suffix=": ") else ""
# get names of objects as character strings:
ndf <- getName(df)
if(length(ndf)>1) ndf <- paste(deparse(substitute(df)), 
                               "[evaluated with getName to ",length(ndf)," lines]")
depsub <- deparse(substitute(x))
nam <- if(substr(depsub,1,11)=="substitute(") deparse(x) else getName(x)
# deal with substitute:
if(substr(nam[1],1,11)=="substitute(") nam <- substr(nam, 12, nchar(nam)-1)
nam <- gsub("\"", "", nam) # if input was a character string
# deal with code input:
# nam assumed to be code if anything else beside 0-9,a-z,.,_ is contained
if(grepl("[^[:alnum:][:space:]\\._]", paste(nam, collapse=" ") ))
 {
 nam1 <- try(eval(x, envir=sys.frames()), silent=TRUE) 
 if(!inherits(nam1, "try-error")) nam <- nam1
 }
# stop if several columns are to be selected:
if(length(nam)>1) stop(calltrace, "Only a single column can be selected ",
                       "with getColumn. The input (",depsub,
                       ") was evaluated to: ", 
                       truncMessage(nam, ntrunc=2, prefix=""), call.=FALSE)

# deal with numeric input:
namnum <- suppressWarnings(as.numeric(nam))
if(!is.na(namnum) && convnum)  
 {
 if(namnum>ncol(df)) stop(calltrace, "column number ", namnum, " is not in '", 
                          ndf, "', which has ", ncol(df), " columns.", call.=FALSE)
 nam <- colnames(df)[namnum]
 }

# check if df is a data.frame
if(!all(class(df)=="data.frame"))
 {
 warning(calltrace, "'",ndf,"' is not a data.frame, but a '", toString(class(df)), 
         "'. Converting with as.data.frame.", call.=FALSE)
 df <- as.data.frame(df)
 }
# check if column exists:
if(length(colnames(df))==0) stop(calltrace, "'",ndf, "' has no columns.", call.=FALSE)
if(!nam %in% colnames(df)) stop(calltrace, "Column '", nam, "' is not in '", ndf,
                           "', which has the columns:",
                           truncMessage(colnames(df), ntrunc=10, prefix=""), 
                           ".", call.=FALSE)
# actually get the column:
out <- df[ , nam]
if(is.null(out) )
  {
  warning(calltrace, "Column '",nam,"' could not be extracted from '", 
          ndf, "'. Returning NULL.", call.=FALSE)
  return(out)
  }
if(NROW(out)==0)
  {
  warning(calltrace, "'",ndf, "' has no rows.", call.=FALSE)
  return(out)
  }
# Further testing:
if(all(is.na(out)) ) warning(calltrace, "Column '", nam, "' in '", ndf, 
                             "' only has NAs.", call.=FALSE)
# return column values
out
}
















# Old stuff from code development:
if(FALSE){

#nam <- if(substr(deparse(substitute(x)),1,10)=="substitute") 
#          as.character(x) else getName(x)

depsub <- deparse(substitute(x))
if(substr(depsub,1,10)=="substitute")
 {
 nam1 <- deparse(x) # to convert symbol, language etc to character
 nam1 <- gsub("\"", "", nam1) # so character string input can be handled
 # try to evaluate in case input was code:
 nam <- try(eval(parse(text=nam1), envir=sys.frames()), silent=TRUE)
 if(inherits(nam, "try-error")) nam <- nam1
 if(!is.character(nam)) nam <- deparse(nam)
 } else
 {
 nam <- try(eval(x, envir=sys.frames()), silent=TRUE)
 if(inherits(nam, "try-error")) nam <- getName(x)
 }

nam <- substr(nam, 12, nchar(nam)-1)
if(substr(depsub,1,10)=="substitute") nam <- as.character(x)

nam <- if(substr(depsub,1,10)=="substitute") as.character(x) else getName(x)

# columns can also come from substitute or be code returning a character:
nam <- try(eval(x, envir=sys.frames()), silent=TRUE)
if(inherits(nam, "try-error")) nam <- getName(x)
if(!is.character(nam)) nam <- deparse(nam) # deal with symbol, language etc
# stop for several columns:
if(length(nam)>1) stop(calltrace, "Only a single column can be selected ",
                       "with getColumn. The input was evaluated to: ",
                       toString(nam), call.=FALSE)
# deal with substitute:
if(substr(nam,1,11)=="substitute(") nam <- substr(nam, 12, nchar(nam)-1)


upp4 <- function(data=iris)
{
force(data)
getName(data)
#colPoints(Sepal.Length, Sepal.Width, Petal.Length, data=myiris, add=FALSE)
}
upp4() # getName(data) fails miserably, see getName


upp3 <- function(cond)
{
myiris <- iris
colPoints(Sepal.Length, Sepal.Width, Petal.Length, data=myiris,
          if(cond) col=seqPal(100, yr=TRUE), add=FALSE)
}
upp3(TRUE) # works
upp3(FALSE) # works (used to fail because col=NULL was not handled well in colPoints)


upp5 <- function(...)
{
myiris <- iris
colPoints(Sepal.Length2, Sepal.Width, Petal.Length, data=myiris, add=FALSE)
}
upp5() # useful abbreviated name message



# Find usage in my packages
setwd(paste0(devtools::as.package(".")$path, "/..")); getwd()
packs <- dir()[-1]

gClines <- pblapply(packs, function(d){
ll <- sapply(dir(paste0(d,"/R"),full.names=TRUE), readLines, simplify=FALSE)
gC <- sapply(ll, grep, pattern="getColumn\\(|colPoints\\(|seasonality\\(|sortDF\\(", simplify=FALSE)
out <- gC[sapply(gC, length)>0]
out <- sapply(out, toString)
names(out) <- tools::file_path_sans_ext(basename(names(out)))
out
})
gClines <- unlist(gClines)
cat(paste0(names(gClines), ": ", gClines), sep="\n", file=newFilename("getColumnUsage.txt"))
}
