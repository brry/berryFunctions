#' @title cross-platform parallel processing with progbar
#' @description Call \code{pbapply::\link[pbapply]{pbsapply}} 
#'              with nc default at number of cores available.
#'              Also, this works on Windows directly.
#' @return vector/matrix, list if simplify=FALSE
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr 2021
#' @seealso \code{pbapply::\link[pbapply]{pbsapply}}, \code{\link{sapply}}, \code{\link{parallelCode}}
#' @keywords iteration
#' @export
#' @examples
#' \dontrun{ # Suppressed on CRAN checks as this is time-consuming
#' fun <- function(x) mean(rnorm(1e7))
#' pbapply::pbsapply(1:20, fun)
#'        par_sapply(1:20, fun)
#'           #sapply(1:20, fun)
#' }
#' 
#' @param X              vector / list of values
#' @param FUN            function to be executed with each element of \code{X}.
#' @param nc             Integer: number of cores to be used in parallel.
#'                       DEFAULT: NULL (available cores)
#' @param pb             Show progress bar with remaining time and at the end runtime? 
#'                       DEFAULT: TRUE
#' @param simplify       Simplify output to vector/matrix if possible? DEFAULT: TRUE
#' @param export_objects For windows: Objects needed in \code{FUN}. DEFAULT: NULL
#' @param \dots          Further arguments passed to \code{FUN} or 
#'                       \code{pbapply::\link[pbapply]{pbsapply}}
#'
par_sapply <- function(
  X, 
  FUN, 
  nc=NULL, 
  pb=TRUE, 
  simplify=TRUE, 
  export_objects=NULL, 
  ...){
# Conditional on pbapply availability:
if(!requireNamespace("pbapply", quietly=TRUE))
  {
  warning("package 'pbapply' is not available, running base::sapply instead.") 
  return(sapply(X=X, FUN=FUN, ..., simplify=simplify))
  }

# Number of cores to use: 
if(is.null(nc)) nc <- parallel::detectCores()

# Progress bar?
if(isFALSE(pb))
  {
  pbo <- pbapply::pboptions(type="none")
  on.exit(pbapply::pboptions(pbo), add=TRUE)
  }

# Easy on non-windows:
if(.Platform$OS.type != "windows")
  return(pbapply::pbsapply(X=X, FUN=FUN, ..., cl=nc, simplify=simplify))

# Slightly more work on windows: 
cl <- parallel::makeCluster(nc)
if(!is.null(export_objects)) parallel::clusterExport(cl, export_objects)
output <- pbapply::pbsapply(X=X, FUN=FUN, ..., cl=cl, simplify=simplify)
parallel::stopCluster(cl)
gc()
return(output)
}
