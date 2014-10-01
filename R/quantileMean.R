
# Berry Boessenkool, Sept 2014

quantileMean <- function(
  x, # Numeric vector whose sample quantiles are wanted
  probs = seq(0, 1, 0.25), # Numeric vector of probabilities with values in [0,1]
  weights=rep(1,9), # Numeric vetor of length 9 with weight for each \code{\link{quantile} method}. Recycled if shorter. DEFAULT: unweighted mean
  names=TRUE, # If TRUE, the result has a names attribute. Set to FALSE for speedup with many probs.
  ...  # further arguments passed to \code{\link{quantile}}, except for type
  )
{
# normalize and cycle weights:
weights <- weights/sum(weights)
weights <- rep(weights, length=9)
# matrix for each prob and type
qx <- sapply(1:9, function(m) quantile(x=x, probs=probs, type=m, names=names, ...))
# weighted mean:
# if length(probs)==1 :
if(is.null(dim(qx)))
  {
  output <- sum(qx*weights)
  if(names) names(output) <- paste0(probs*100,"%")
  }
else
output <- apply(qx, 1, function(y) sum(y*weights))
output
}
