#' insert rows to data.frame
#' 
#' Insert (multiple) rows to a data.frame, possibly coming from another data.frame, with value and row recycling
#' 
#' @return data.frame
#' @note Has not yet been tested with RWI (really weird input), so might not be absolutely foolproof
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2015, based on code by Ari B. Friedmann
#'         (I added the for loop, recycling, input controls and data.framification)
#' @seealso \code{\link{addRows}}, \code{\link{sortDF}}
#' @references \url{https://stackoverflow.com/questions/11561856/add-new-row-to-dataframe}
#' @keywords misc manip array
#' @export
#' @examples
#' 
#' existingDF <- as.data.frame(matrix(1:20, nrow=5, ncol=4))
#' existingDF
#' insertRows(existingDF, 2) # default new=NA is recycled
#' insertRows(existingDF, 2, rcurrent=TRUE) # after current line, not at it
#' insertRows(existingDF, 2, 444:446)
#' insertRows(existingDF, 3, new=matrix(10:1,ncol=2)) # input warning
#' insertRows(existingDF, 1)
#' insertRows(existingDF, 5)
#' insertRows(existingDF, 6) # use addRows for this:
#' addRows(existingDF, n=1)
#' insertRows(existingDF, 9) # pads NA rows inbetween
#' 
#' # Works for multiple rows as well:
#' insertRows(existingDF, r=c(2,4,5), new=NA, rcurrent=TRUE)
#' insertRows(existingDF, r=c(2,4,5), new=NA)
#' insertRows(existingDF, r=c(2,4,4), new=NA)
#' insertRows(existingDF, r=c(2,4,4), new=NA, rcurrent=TRUE)
#' 
#' # Also works with a data.frame for insertion:
#' insertDF <- as.data.frame(matrix(101:112, nrow=3, ncol=4))
#' insertRows(existingDF, 3, new=insertDF) # excess rows in new are ignored
#' insertRows(existingDF, c(2,4,5), new=insertDF)
#' insertRows(existingDF, c(2,4:6), new=insertDF) # rows are recycled
#' 
#' @param df data.frame
#' @param r   Row number (not name!), at which the \code{new} row is to be inserted.
#'            Can be a vector.
#' @param new Vector with data to be inserted, is recycled.
#'            Alternatively, a data.frame, whose rows are put into the r locations.
#'            If it has more rows than length(r), the excess rows are ignored.
#'            DEFAULT: NA
#' @param rcurrent Logical: should \code{r} specify the current rows of \code{df},
#'            after which \code{new} is to be appended?
#'            If FALSE (the default for backwards compatibility), the rownumbers 
#'            of the output (instead of the input) are \code{r}. I.e. \code{new} is 
#'            inserted \emph{at}, not \emph{after} the rownumber. DEFAULT: FALSE
insertRows <- function(
df,
r,
new=NA,
rcurrent=FALSE
)
{
# Input checks:
if(!is.data.frame(df)) warning("df is not a data.frame.")
if(!(is.vector(new) | is.data.frame(new))) warning("new row is not a vector or data.frame.")
# how should r be interpreted?
if(rcurrent) r <- r + replace(seq_along(r),1,2)-1
# recycle new row values:
if(!is.data.frame(new))
  {
  new <- rep(new, length=ncol(df)) # recycle vector to number of columns
  new <- rbind(new) # make it into a data.frame (well, a matrix, but that works fine, too)
  }
# recycle the rows:
new <- new[rep(1:nrow(new), length=length(r)), , drop=FALSE]
# for each value in r, create a row with the i-th row of new:
for(i in 1:length(r))
  {
  SEQ <- seq(from=r[i], to=nrow(df))
  df[SEQ+1,] <- df[SEQ,]
  df[r[i],] <- new[i,]
  }
# return the final output:
df
}
