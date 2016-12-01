#' Convert list of arrays to array
#'
#' Convert a list of arrays to a single array, conserving names
#'
#' @return array
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2016
#' @seealso \code{\link{l2df}}, \code{\link{help}}
#' @keywords list manip array
#' @export
#' @examples
#' A1 <- array(1:24, dim=c(4,2,3), dimnames=list(my_x=paste0("row",1:4), my_y=c("A","B"), paste0("n",1:3)))
#' A1
#' # Selection:
#' A1[,,"n2"]
#' A1["row2",,] # result rotated against expectation
#' A1[,"A",]
#' # aggregation:
#' apply(A1, MARGIN=1:2, FUN=sum) # keep first two dimensions
#' apply(A1, MARGIN=c(1,3), FUN=sum) # aggregate over my_y -> row1: 6, 22, 38
#' A1["row1",,]                                    # 1+5=6, 9+13=22, 17+21=38
#' 
#' as.vector(A1)
#' A2 <- A1+2
#' A3 <- A1+4
#' 
#' LA <- list(A1=A1, A2=A2, A3=A3) # list of arrays
#' 
#' AA <- l2array(LA)
#' AA
#' AA[,,,"A2"]
#' avg <- apply(AA, MARGIN=1:3, mean)
#' stopifnot(all(avg==A2))
#' 
#' # names check:
#' LN <- LA
#' dimnames(LN[[2]]) <- list(my_x=paste0("row",1:4), my_y=c("A","B"), intentional=paste0("n",1:3))
#' AN <- l2array(LN)
#' 
#' LN <- LA
#' rownames(LN[[3]])[2] <- "intentional_diff"
#' A <- l2array(LN)
#' 
#' # data type check
#' A <- l2array(c(LA, 999))
#' 
#'
#' @param x List with arrays. The dimension of the first is target dimension.
#' @param usenames   Logical: Should names be kept? The names of the first element 
#'                   will be used. DEFAULT: TRUE
#' @param checknames Logical: Should names of other elements be the same as in x[[1]]?
#'                   Will be checked with useful warning messages. DEFAULT: TRUE
#'
l2array <- function(
x,
usenames=TRUE,
checknames=TRUE
)
{
# input checks:
if(!is.list(x)) stop("x must be a list, not a ", class(x))
isar <- sapply(x, class)=="array"
if(!isar[1]) stop("x[[1]] must be an array, not a ", class(x[[1]]))
if(!all(isar)) warning("all elements in x should be arrays. The following are not: ",
                       toString(which(!isar)), "\n Please inspect your output carefully! ",
                       "The first values may be recycled at the end.")
# dimension names:
dina <- dimnames(x[[1]]) 
elna <- names(x)
# check names:
if(checknames && length(x)>1)
  {
  # check names of dimnames:
  dummy <- lapply(2:length(x), function(i){
  dina2 <- dimnames(x[[i]])
  ncomp <- names(dina) == names(dina2)
  if(!all(ncomp)) warning("in l2array: dimnames are not equal to output names in x[[", i, "]]:\n  '",
                       toString(names(dina2)), "' instead of '", toString(names(dina)), "'.", call.=FALSE)
  })
  # check array dimnames:
  dummy <- lapply(2:length(x), function(i){
  dina2 <- dimnames(x[[i]])
  dummy <- sapply(seq_along(dina), function(j){
  ncomp <- dina[[j]] == dina2[[j]]
  if(!all(ncomp)) warning("in l2array: dimnames are not equal to output names in x[[", i, "]]:\n  '",
                       toString(dina2[[j]]), "' instead of '", toString(dina[[j]]), "'.", call.=FALSE)
  })})
  }
outnames <- NULL
if(usenames) outnames <- c(dina, list(elna))
out <- array(unlist(x), dim=c(dim(x[[1]]),length(x)), dimnames=outnames)
out
}
