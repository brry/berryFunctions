#' seq with a range argument
#' 
#' sequence given by range or vector of values.
#' 
#' @return Numeric vector.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb 2014
#' @seealso \code{\link{seq}}, \code{\link{range}},
#'          \url{https://web.archive.org/web/20190107005108/https://r.789695.n4.nabble.com/seq-range-argument-td4684627.html}
#' @keywords datagen
#' @importFrom grDevices extendrange
#' @importFrom utils tail
#' @export
#' @examples
#' 
#' seqR(range=c(12,6), by=-2)
#' m <- c(41, 12, 38, 29, 50, 39, 22)
#' seqR(m, len=6)
#' # Takes min and max of range if the vector has more than two elements.
#' 
#' seqR(range=c(12,6), by=-2, extend=0.1)
#' # internaly calls extendrange with f=extend
#' 
#' @param range  vector with 2 values (1st taken as \code{from}, 2nd as \code{to}) 
#'               or more (the result is then always ascending).
#' @param from   start value of sequence. DEFAULT: NA (determined from range)
#' @param to     end value of sequence. DEFAULT: NA (determined from range)
#' @param extend Factor \emph{f} passed to \code{\link{extendrange}}. DEFAULT: 0
#' @param warn   Logical: warn about non-numeric classes? DEFAULT: TRUE
#' @param \dots  further arguments passed to \code{\link{seq}}.
#' 
seqR <- function(
range,
from=NA,
to=NA,
extend=0,
warn=TRUE,
...)
{
# only set from and to if range is given as input:
if(!missing(range))
  {
  # Input checking:
  if(!is.numeric(range) & warn) warning("'range' is not numeric, but ", 
                                        toString(class(range)),".")
  # accept long vectors:
  if(length(range)>2) range <- base::range(range, na.rm=TRUE)
  # actual work:
  range <- extendrange(r=range, f=extend)
  from <- range[1]     # first
  to <- tail(range,1)  # and last value
  }
# now call seq with from and to (obtained from range)
seq(from=from, to=to, ...)
}
