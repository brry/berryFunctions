#' Average of R's quantile methods
#' 
#' Weighted average of R's quantile methods
#' 
#' @details weights are internally normalized to sum 1
#' 
#' @return numeric named vector, as returned by \code{\link{apply}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sept 2014
#' @seealso \code{\link{quantile}}
#' @keywords univar
#' @importFrom stats quantile
#' @export
#' @examples
#' 
#' exDat <- rnorm(30,sd=5)
#' quantile(exDat, probs=c(0.9, 0.99), type=1)
#' quantile(exDat, probs=c(0.9, 0.99), type=2)
#' round( sapply(1:9, function(m) quantile(exDat, probs=0.9, type=m)) , 3)
#' # and now the unweighted average:
#' quantileMean(exDat, probs=c(0.9, 0.99))
#' quantileMean(exDat, probs=0.9)
#' # say I trust type 2 and 3 especially and want to add a touch of 7:
#' quantileMean(exDat, probs=c(0.9, 0.99), weights=c(1,5,5,0,1,1,3,1,1))
#' 
#' # quantile sample size dependency simulation:
#' qbeta(p=0.999, 2, 9) # dist with Q99.9% = 0.62
#' betaPlot(2, 9, cumulative=FALSE, keeppar=TRUE)
#' abline(v=qbeta(p=0.999, 2, 9), col=6, lwd=3)
#' qm <- function(size) quantileMean(rbeta(size, 2,9), probs=0.999, names=FALSE)
#' n30  <- replicate(n=500, expr=qm(30))
#' n1000 <- replicate(n=500, expr=qm(1000))
#' lines(density(n30)) 
#' lines(density(n1000), col=3) 
#' # with small sample size, high quantiles are systematically
#' # underestimated. for Q0.999, n must be > 1000
#' 
#' 
#' \dontrun{
#' # #Excluded from CRAN Checks because of the long computing time
#' 
#' # Parametrical quantiles can avoid sample size dependency!
#' library2("extremeStat")
#' library2("pbapply")
#' 
#' dlq <- distLquantile(rbeta(1000, 2,9), probs=0.999, list=TRUE, gpd=FALSE)
#' plotLquantile(dlq, nbest=10) # 10 distribution functions
#' select <- c("wei","wak","pe3","gno","gev","gum","gpa","gam")
#' 
#' # median of 10 simulations:
#' nsim <- 10 # set higher for less noisy image (but more computing time)
#' qmm <- function(size, truncate=0) median(replicate(n=nsim,
#'        expr=quantileMean(rbeta(size, 2,9), probs=0.999, names=FALSE, 
#'                          truncate=truncate)                            ))
#' 
#' pqmm <- function(size, truncate=0) median(replicate(n=nsim,
#'        expr=mean(distLquantile(rbeta(size, 2,9), probs=0.999, selection=select,
#'                  progbars=FALSE, time=FALSE, truncate=truncate, gpd=FALSE, 
#'                  weighted=FALSE, empirical=FALSE, ssquiet=TRUE)[1:8, 1])   ))
#'                  
#' n <- round(  logSpaced(min=10, max=1000, n=15, base=1.4, plot=FALSE)  )
#' 
#' medians_emp <- pbsapply(n, qmm)  # medians of regular quantile average
#' # with truncation, only top 20% used for quantile estimation (censored quant):
#' medians_emp_trunc <- sapply(n, qmm, truncate=0.8) 
#' # medians of parametrical quantile estimation
#' medians_param       <- pbsapply(n, pqmm)              # takes ~60 secs
#' medians_param_trunc <- pbsapply(n, pqmm, truncate=0.8)
#' 
#' plot(n, medians_emp, type="l", ylim=c(0.45, 0.7), las=1)
#' abline(h=qbeta(p=0.999, 2, 9), col=6) # real value
#' lines(n, medians_emp_trunc, col=2) #  don't help!
#' # In small samples, rare high values, on average, simply do not occur 
#' lines(n, medians_param, col=4) # overestimated, but not dependent on n
#' # with truncation, only top 20% used for quantile estimation
#' lines(n, medians_param_trunc, col="orange", lwd=3) # much better!
#' }
#' 
#' @param x Numeric vector whose sample quantiles are wanted
#' @param probs Numeric vector of probabilities with values in [0,1]. DEFAULT: seq(0, 1, 0.25)
#' @param weights Numeric vector of length 9 with weight for each \code{\link{quantile} method}.
#'        Recycled if shorter. DEFAULT: unweighted mean. DEFAULT: rep(1,9)
#' @param names If TRUE, the resulting vector has a names attribute. DEFAULT: TRUE
#' @param truncate Number between 0 and 1. Censored quantile: fit to highest values only (truncate lower proportion of x). Probabilities are adjusted accordingly. DEFAULT: 0
#' @param \dots further arguments passed to \code{\link{quantile}}, except for type
#' 
quantileMean <- function(
x,
probs = seq(0, 1, 0.25),
weights=rep(1,9),
names=TRUE,
truncate=0,
...
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
