#' GOF measures
#'
#' Goodness of Fit measures (GOF) for two vectors.\cr
#' \bold{gofNA}: not exported, checks input for each of the functions:\cr
#' \bold{rsquare}: Coefficient of determination (R2)\cr
#' \bold{rmse}: Root Mean Square Error (for minimising in \code{\link{optim}})\cr
#' \bold{nse}: Nash-Sutcliffe efficiency, based on RHydro::eval.NSeff\cr
#' \bold{kge}: Kling-Gupta efficiency (better than NSE), 
#'             based on hydroGOF::KGE, where there are many more options
#' 
#' @name gof
#' @aliases rsquare rmse nse kge
#'
#' @return Single numerical value
#' 
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sept 2016
#' @note NAs are omitted with warning.
#' @seealso \code{\link{cor}}, \code{\link{lm}}.
#'             \url{http://en.wikipedia.org/wiki/R-squared}, 
#'             \url{http://en.wikipedia.org/wiki/Mean_squared_error}
#' @keywords ts univar
#' @importFrom stats cor
# @export # do not export gofNA
#' @examples
#' # R squared and RMSE --------------------------------------------------------
#' set.seed(123)
#' x <- rnorm(20)
#' y <- 2*x + rnorm(20)
#' plot(x,y)
#' legGOF <- function(a,b)
#'   {
#'   text(a,b, paste(c("      R2","RMSE","  NSE","  KGE"), collapse="\n"), adj=1.2)
#'   text(a,b, paste(round(c(rsquare(x,y), rmse(x,y), nse(x,y), kge(x,y)),5), 
#'                   collapse="\n"), adj=0)
#'   }
#' legGOF(-1.5, 2) # R2 good, but does not check for bias (distance from 1:1 line)
#' 
#' abline(a=0,b=1) ; textField(-1.5,-1.5, "1:1")
#' abline(lm(y~x), col="red")
#' p <- predict(lm(y~x))
#' points(x, p, pch=3, col="red")
#' segments(x, y, x, p, col="red")
#' stopifnot(all.equal(  nse(y,p) , rsquare(y,x)  ))
#' 
#' 
#' # Input checks
#' is.error(   rmse(1:6, 1:8)    ,  tell=TRUE)
#' nse(replace(x,3,NA), y)
#' kge(rep(NA,20), y)
#' rmse(0,0, quiet=TRUE)
#' rsquare(1:6, tapply(chickwts$weight, chickwts$feed, mean) )
#' 
#' \dontrun{ # time consuming Simulation
#' r2 <- sapply(1:10000, function(i){
#'    x <- rnorm(20);  y <- 2*x + rnorm(20);  rsquare(x,y) })
#' hist(r2, breaks=70, col=5,
#' main= "10'000 times   x <- rnorm(20);  y <- 2*x + rnorm(20);  rsquare(x,y)")
#' # For small samples, R^2 can by chance be far off the 'real' value!
#' }
#'
#' # NSE and KGE ---------------------------------------------------------------
#' 
#' y <- dbeta(1:40/40, 3, 10) # simulated
#' x <- y + rnorm(40,0,sd=0.2) # observed
#' plot(x)
#' lines(y, col="blue")
#' legGOF(25, 2)
#' rmse(x,y) ; rmse(y,x)
#' nse(x,y) ; nse(y,x)  # x=obs, y=sim  (second command is wrong)
#' kge(x,y) ; kge(y,x)
#' 
#'
#' @param a Numerical vector with observational data
#' @param b Simulated data (to be compared to a)
#' @param quiet Should NA-removal warnings be suppressed? 
#'              This may be helpful within functions. DEFAULT: FALSE
#' @param fun Character string with function name for error and warning messages


# gofNA for processing NAs, Sept 2016 ------------------------------------------

gofNA <- function(a, b, quiet=FALSE, fun="")
{
# Input checks on type and length:
if(fun!="") fun <- paste0(fun, ": ")
if(!(is.vector(a) & is.vector(b))) warning(fun, "inputs are not vectors, but: ", 
                                           class(a), " and ", class(b), call.=FALSE)
if(length(a) != length(b)) stop(fun, "vectors not of equal length, but: ", 
                                length(a), " and ", length(b), call.=FALSE)
# NA checks
if( anyNA(a) | anyNA(b) )
{
  Na <- which(is.na(a)|is.na(b))
  if(!quiet) warning(fun, length(Na), " NAs were omitted from ", length(a), 
                     " data points (",round(length(Na)/length(a)*100,1),"%).", call.=FALSE)
  a <- a[-Na] ; b <- b[-Na]
} 
# zero check:
if(all(a==0) | all(b==0))
{
  if(!quiet) warning(fun, "all a (or all b) values are zero, returning NA.", call.=FALSE)
  return(NULL)
} # end if zero
# Output:
data.frame(a,b)
}



# Coefficient of determination (rsquare), 2014 ---------------------------------
#' @export
#' @rdname gof
#' @keywords ts univar

rsquare <- function(a, b, quiet=FALSE)
{
g <- gofNA(a, b, quiet=quiet, fun="rsquare")
if(is.null(g)) return(NA)
cor(g$a,g$b)^2
# Using cor is much faster than using
# aa <- a-mean(a);    bb <- b-mean(b);      sum(aa*bb)^2/sum(aa^2)/sum(bb^2)
}



# Root Mean Squared Error (RMSE), 2014 -----------------------------------------
#' @export
#' @rdname gof

rmse <- function(a, b, quiet=FALSE)
{
g <- gofNA(a, b, quiet=quiet, fun="rmse")
if(is.null(g)) return(NA)
sqrt( sum((g$a-g$b)^2)/length(g$b) )
}



# Nash-Sutcliffe efficiency (NSE), July 2013 -----------------------------------
#' @export
#' @rdname gof

nse <- function(a, b, quiet=FALSE) # based on RHydro::eval.NSeff
{
g <- gofNA(a, b, quiet=quiet, fun="nse")
if(is.null(g)) return(NA)
1 - ( sum((g$a - g$b)^2) / sum((g$a - mean(g$a))^2) )
}



# Kling-Gupta efficiency (KGE), Sept 2016 --------------------------------------
#' @export
#' @rdname gof

kge <- function(a, b, quiet=FALSE) # based on hydroGOF::KGE
{
g <- gofNA(a, b, quiet=quiet, fun="kge")
if(is.null(g)) return(NA)
1 - sqrt(   ( cor(g$b,      g$a) - 1 )^2  +      # correlation
            (  sd(g$b)/  sd(g$a) - 1 )^2  +      # variability
            (mean(g$b)/mean(g$a) - 1 )^2     )   # bias
}




# slow r2 ----------------------------------------------------------------------

if(FALSE)
{
# rsquare2: 3.4 instead of 2.1 seconds (with 1e8 in the example below)
# crucial difference if calculations are done iteratively or performed multiple times
  
rsquare2 <- function(a, b, quiet=FALSE) 
{ 
g <- gofNA(a, b, quiet=quiet, fun="rsquare2")
if(is.null(g)) return(NA)
aa <-  a-mean(a)
bb <-  b-mean(b)
sum(aa*bb)^2/sum(aa^2)/sum(bb^2) 
}
  
a <- sort(rnorm(1e7)); b <- 2*a+3+rnorm(length(a))
system.time(rsquare(a,b))
system.time(rsquare2(a,b))
}