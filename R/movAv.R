# Moving Average.
# Berry Boessenkool, ca 2012

movAv <- function(
                  dat,
                  width=7,
                  weights=rep(1,width) )
{
# input-checking (added May 2014 along with new option weights) :
dat <- as.vector(dat)
if(!is.vector(dat))   stop("Dat has to be a vector.")
if(missing(weights))  weights <- rep(1,width)
if(missing(width))    width <- length(weights)
if(width != length(weights))  stop("Width and length of weights are not equal!")
if(ceiling(width) != floor(width))  stop("Width has to be an (odd) integer.")
##if(length(weights) %% 2 == 0) stop("Length of weights must be an odd number!")
if(width %% 2 == 0)
   {
   warning("even width (", width, ") is changed to odd width (", width+1, ").",
            if(!missing(weights)) "\nWeights are now set uniformly!")
   width <- width+1
   weights <- rep(1,width)
   }
if(length(weights) >= length(dat))
   stop("weight vector too long for this dataset.")

# Easy output (don't run all the code in this case):
if(round(width)==1) return(dat)

# normalize weights to sum 1
weights <- weights/(sum(weights))

# Half the width in each direction:
s <- floor(width/2) # s: steps in each direction
# Length of input vector:
n <- length(dat)

# Average of window around each value:
#v <- sapply( (s+1):(n-s),  function(i) sum(weights*dat[(i-s):(i+s)]) )
# stop("missing values must be taken into consideration!")

v <- sapply( (s+1):(n-s),  function(i)
  {
  subset <- dat[(i-s):(i+s)]
  if(any(is.na(subset)))
     {weights[is.na(subset)] <- 0
     weights <- weights/(sum(weights))
     }
  sum(weights*subset, na.rm=TRUE)
  })

# Append s NAs at each margin (Half a window at each side):
v <- c(rep(NA, s), v, rep(NA, s) )
# 1:s and (n-s+1):n elements at margins remain NA

# Error checking:
if(length(v) != length(dat)) stop("Window size was computed wrongly.
Please report conditions: berry-b@gmx.de")
# Output
return(v)
}

# Old version with unweighted average:
#v <- sapply( (s+1):(n-s),  function(i) mean(dat[(i-s):(i+s)], na.rm=TRUE) )
# Old warning, now error:
#      if(!missing(weights)) "\nWeights are now set uniformly!"
#         weights <- rep(1,width)

