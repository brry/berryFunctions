#' Vectorized testing for near-equality
#'
#' Vectorized testing for near-equality with \code{\link{all.equal}}.
#' Since elements are recycled, this will not work for environments.
#' You \emph{can} use \code{almost.equal} directly in \code{if} expressions.
#'
#' @return Logical vector
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2017
#' @seealso \code{\link{all.equal}}
#' @export
#' @examples
#'
#' # General usage:
#' x <- c(0.4-0.1, 0.5-0.2)
#' x
#' x==0.3                    # FALSE TRUE # but mathematically, x is 0.3
#' all.equal(x, rep(0.3,2))  # TRUE
#' almost.equal(x,0.3)       # TRUE TRUE  # nice
#'
#' y <- c(7777, 0.3)
#'    all.equal(x,y) # "Mean relative difference: 25922.33"   Not what I want
#' almost.equal(x,y) # FALSE TRUE                             Exactly what I want
#'
#'
#' # Testing vectorization
#' almost.equal(1:6, 3)
#' almost.equal(1:6, NA)
#' almost.equal(1:6, NULL)
#'
#'
#' # Testing the function for different data types (in order of coercion):
#' almost.equal(c(TRUE,FALSE,NA), c(TRUE,FALSE,NA))      # logical
#' almost.equal(as.factor(letters), as.factor(letters))  # factor
#'    all.equal(1:6, 1:6)
#' almost.equal(1:6, 1:6)                                # integer  numeric see above
#' 0.4+0.4i - 0.1-0.1i == 0.3+0.3i
#' almost.equal(0.4+0.4i - 0.1-0.1i, 0.3+0.3i)           # complex
#'    all.equal(letters, tolower(LETTERS))
#' almost.equal(letters, tolower(LETTERS))               # character
#' almost.equal(Sys.Date()+1:4,Sys.Date()+1:4)           # Date
#' x <- Sys.time()+0:2
#' all.equal(x,x)
#' almost.equal(x,x)                                     # POSIXt
#' A <- list(a=1:5, b=0.5-0.2)
#' B <- list(a=1:5, b=0.4-0.1)
#'    all.equal(A,B)
#' almost.equal(A,B)                                     # list
#'
#'
#' @param x,y R objects to be compared with each other, recycled to max length
#' @param \dots Further arguments passed to \code{\link{all.equal}}
#'
almost.equal <- function(
x,
y,
...
)
{
goodclass <- c("logical","factor","integer","numeric","Date","complex","character","list","POSIXt")
goodclass <- any(class(x) %in% goodclass) && any(class(y) %in% goodclass)
if(!goodclass) warning("Input class is '", toString(class(x)), "' and '", toString(class(y)),
                        "'. Things may go wrong.")
len <- max(length(x), length(y), na.rm=TRUE)
x <- rep(x, length.out=len)
y <- rep(y, length.out=len)
sapply(seq_len(len), function(i) isTRUE(all.equal(x[i],y[i])))
}



