
# Berry Boessenkool, Sept 2014

quantileMean <- function(
  x, # Numeric vector whose sample quantiles are wanted
  probs = seq(0, 1, 0.25), # Numeric vector of probabilities with values in [0,1]
  weights=rep(1,9), # Numeric vetor of length 9 with weight for each \code{\link{quantile} method}. Recycled if shorter. DEFAULT: unweighted mean
  names=TRUE, # If TRUE, the resulting vector has a names attribute.
  truncate=0, # Number between 0 and 1. Censored quantile: fit to highest values only (truncate lower proportion of x). Probabilities are adjusted accordingly.
  ...  # further arguments passed to \code{\link{quantile}}, except for type
  )
{
# input checks:
truncate <- truncate[1] # cannot be vectorized
if(truncate<0 | truncate>=1) stop("truncate must be a number between 0 and 1.")
probs2 <- probs
# truncation:
if(truncate!=0)
  {
  x <- sort(x)[ -1:-(truncate*length(x)) ]
  probs2 <- (probs-truncate)/(1-truncate)
  probs2[probs < truncate] <- 0
  }
# normalize and cycle weights:
weights <- rep(weights, length=9)
weights <- weights/sum(weights)
# matrix for each prob and type:
qx <- sapply(1:9, function(m) quantile(x=x, probs=probs2, type=m, names=FALSE, ...))
if(length(probs2)==1) qx <- t(qx)
# weighted mean:
output <- apply(qx, 1, function(y) sum(y*weights))
if(names) names(output) <- paste0(probs*100,"%")
output[probs<truncate] <- NA
output
}
