# Add a range argument to seq
# Berry Boessenkool, berry-b@gmx.de, 2014-02-03

seqR <- function(
                 range,
                 from=1,
                 to=1,
                 extend=0, # f-factor passed to extendrange
                 ...)
{
# only set from and to if range is given as input:
if(!missing(range)) 
  {
  # Input checking:
  if(!is.vector(range)) stop("'range' must be a vector.")
  if(!is.numeric(range)) stop("'range' must be numeric.")
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
