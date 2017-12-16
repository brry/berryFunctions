#' @title code chunk for parallelization
#' @description message a code chunk template for parallelization with progress bar on windows.
#' On Linux, just use \code{pblapply(X, cl=8, FUN=fun)}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @export
#' @examples
#' parallelCode()
#' 
parallelCode <- function()
{
message(
'# Pseudo function and objects:
obj1 <- obj2 <- obj3 <- "someStuff"
fun <- function(x) nchar(obj1) + mean(rnorm(1e5))
input <- 1:100

library(pbapply); library(parallel) # for parallel lapply execution
cl <- makeCluster( detectCores()-1 )
clusterExport(cl, c("obj1", "obj2", "obj3"))
output <- pbsapply(X=input, cl=cl, FUN=fun, simplify=FALSE)
stopCluster(cl); rm(cl); gc()
'
)
}
