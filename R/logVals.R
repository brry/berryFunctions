# Function to get log-axis values and labels
# berry-b@gmx.de, Feb 2014, idea 2013

logVals <- function(
  from=-7,               # lower exponent OR vector with data
  to=7,                  # high end
  Range,                 # or give from and to as range
  base=1,                # bases to be used, eg. c(1,2,5)
  big.mark="'",          # symbol separating thousands, eg. space, comma, dot, etc. see "format" and "prettyNum"
  decimal.mark=".",      # character separating comma values, see "format" and "prettyNum"
  scientific=FALSE,      # see "format"
  exponent=Inf,          # like scipen, but for expression (to be used in logAxis)
  expobase1=FALSE,       # Should "n * " be appended before 10^exp if n=1?
  allbase=1:9            # base for \code{$all} (for horizontal lines)
  )
{
# Calculate the exponents from vector, if given as first argument:
if( missing(to)  &  NROW(from)>1  )
  {
  rng <- range(log10(from[from>0]), finite=TRUE)
  from <- floor(rng[1])
  to <- ceiling(rng[2])
  }
# or calculate the exponents from range, if given
if( !missing(Range)  )
  {
  from <- floor(Range[1])
  to <- ceiling(Range[2])
  }
# values for lines and labels:
vals <- base*10^rep(floor(from):ceiling(to), each=length(base))
# formatted values for labels:
labs <-  format(vals, big.mark=big.mark, trim=TRUE, scientific=scientific, 
                      drop0trailing=TRUE, decimal.mark=decimal.mark)
# change to expression if value > exponent :
change1 <- abs(log10(vals)) >= exponent  & log10(vals)%%1 ==0 # base=1
change2 <- abs(log10(vals)) >= exponent  & log10(vals)%%1 !=0 # other base
if(expobase1)
  {
  change2 <- change1 | change2        # all should be treated as other bases
  change1 <- rep(FALSE, length(vals))
  }
if(any(change1 | change2)) # only deal with expression if applicable:
  {
  w2c1 <- which(change1)  # w2c= which to change
  w2c2 <- which(change2)
  wnc <- which(!change1 & !change2)
  labs2 <- vector("expression", length(labs))
  for(i in w2c1) labs2[[i]] <- bquote(10^.(floor(log10(vals))[i]))
  for(i in w2c2) labs2[[i]] <- bquote(
         .(rep(base, length.out=length(vals))[i])%.%10^.(floor(log10(vals))[i]))
  for(i in wnc) labs2[[i]] <- labs[i]  # regular labels for all entries not
  labs <- labs2                        # affected by the exponent size rule
  }
# Values for lines:
all <- allbase * 10^rep(floor(from):ceiling(to), each=length(allbase))
# return end result
list(vals=vals, labs=labs, all=all)
}
