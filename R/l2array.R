#' Convert list of arrays to array
#' 
#' Convert a list of arrays to a single array, conserving names.
#' If dimnames do not need to be checked, you can also directly use \cr
#' \code{do.call(abind::abind, list(LIST, rev.along=0, use.dnns=TRUE)) }
#' 
#' @return array
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2016
#' @seealso \code{\link{l2df}}, \code{\link{help}},
#'          \url{https://stackoverflow.com/a/4310747}
#' @keywords list manip array
#' @importFrom abind abind
#' @export
#' @examples
#' 
#' LISTm <- lapply(list(1:6,7:12,13:18,19:24), matrix, ncol=3,
#'                dimnames=list(x=c("a","b"), y=c("i","j","k"))  )
#' l2array(LISTm)
#' 
#' LIST <- lapply(LETTERS[1:5], function(x) array(paste0(x,1:24), dim=c(3,4,2)))
#' str(LIST)
#' LIST[[2]]
#' LISTa1 <- l2array(LIST)
#' LISTa1
#' str(LISTa1)
#' 
#' # The old l2array (<1.13.14, 2017-01-06) was very slow on large lists.
#' # I then found abind, which is much much much faster and easier on memory!
#' # It now replaces the internal old actual conversion code
#' # l2array still checks the dimnames
#' LISTa2 <- do.call(abind::abind, list(LIST, rev.along=0, use.dnns=TRUE))
#' LISTa2
#' stopifnot(all(LISTa1==LISTa2))
#' rm(LIST, LISTa1, LISTa2)
#' 
#' 
#' # list of dataframes:
#' LDF <- list(IR1=iris[1:5,1:2], IR2=iris[11:15,1:2], IR3=iris[21:25,1:2])
#' l2array(LDF)
#' 
#' 
#' 
#' # General intro to arrays -----
#' 
#' A1 <- array(1:24, dim=c(4,2,3), dimnames=list(
#'                    my_x=paste0("row",1:4), my_y=c("A","B"), paste0("n",1:3)))
#' A1
#' which(A1==20, arr.ind=TRUE)
#' 
#' # Selection:
#' A1[,,"n2"]
#' A1[,,1:2]
#' A1["row2",,] # result rotated against expectation -> transpose with t(...)
#' A1[,"A",]
#' # aggregation:
#' apply(A1, MARGIN=1:2, FUN=sum) # keep first two dimensions
#' apply(A1, MARGIN=c(1,3), FUN=sum) # aggregate over my_y -> row1: 6, 22, 38
#' A1["row1",,]                                    # 1+5=6, 9+13=22, 17+21=38
#' 
#' as.vector(A1)
#' 
#' A <- array(1:24, dim=c(3,4,2), dimnames=list(x=paste0("x",1:3),
#'                                              y=paste0("y",1:4),
#'                                              z=paste0("z",1:2)))
#' str(A)
#' rm(A)
#' 
#' 
#' # l2array -----
#' 
#' A2 <- A1+2
#' A3 <- A1+4
#' LIST <- list(A1=A1, A2=A2, A3=A3) # list of arrays
#' 
#' LA <- l2array(LIST)
#' LA
#' str(LA)
#' LA[,,,"A2"]
#' LA["row2", ,"n2",]
#' avg <- apply(LA, MARGIN=1:3, mean)
#' stopifnot(all(avg==A2))
#' 
#' 
#' # names check -----
#' 
#' LISTN <- LIST
#' names(dimnames(LISTN[[2]]))[3] <- "intentional"
#' dimnames(LISTN[[3]])[3] <- list(paste0("k",1:3))
#' LAN <- l2array(LISTN)
#' LAN["row2", ,"k2",] # n2 is now changed to k2
#' LANa <- do.call(abind::abind, list(LISTN, rev.along=0, use.dnns=TRUE))
#' all(LAN==LANa)
#' str(LANa)
#' 
#' LISTN <- LIST
#' rownames(LISTN[[3]])[2] <- "intentional_diff"
#' LAN <- l2array(LISTN)
#' 
#' # data type check
#' is.error(   A <- l2array(c(LA, 999)),  tell=TRUE, force=TRUE)
#' 
#' 
#' @param x List with arrays/data.frames. The dimension of the first is target dimension.
#' @param \dots Further arguments passed to \code{abind::\link[abind]{abind}}
l2array <- function(
x,
...)
{
# input checks:
if(!is.list(x)) stop("x must be a list, not a ", class(x))
alldf <- all(sapply(x, inherits, "data.frame"))
if(alldf) x <- lapply(x, as.matrix)
isar <- sapply(x, class) %in% c("matrix","array")
if(!isar[1]) stop("x[[1]] must be an array, not a ", class(x[[1]]))
if(!all(isar)) warning("all elements in x should be arrays. The following are not: ",
                       toString(which(!isar)), "\n Please inspect your output carefully! ",
                       "The first values may be recycled at the end.")
# dimension names:
dina <- dimnames(x[[1]])
elna <- names(x)
# check names:
if(length(x)>1)
  {
  ###message("Checking dimnames names..."); flush.console()
  # check names of dimnames:
  dummy <- lapply(2:length(x), function(i){
  dina2 <- dimnames(x[[i]])
  ncomp <- names(dina) == names(dina2)
  if(!all(ncomp)) warning("in l2array: names of dimnames are not equal to output names in x[[", i, "]]:\n  '",
                       toString(names(dina2)), "' instead of '", toString(names(dina)), "'.", call.=FALSE)
  })
  # check array dimnames:
  ###message("Checking dimnames..."); flush.console()
  dummy <- lapply(2:length(x), function(i){
  dina2 <- dimnames(x[[i]])
  dummy <- sapply(seq_along(dina), function(j){
  ncomp <- dina[[j]] == dina2[[j]]
  if(!all(ncomp)) warning("in l2array: dimnames are not equal to output names in x[[", i, "]]:\n  '",
                       toString(dina2[[j]]), "' instead of '", toString(dina[[j]]), "'.", call.=FALSE)
  })})
  }
# actually convert to array:
out <- do.call(abind::abind, list(x, rev.along=0, use.dnns=TRUE, ...))
out
}
