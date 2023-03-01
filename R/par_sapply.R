#' @title cross-platform parallel processing with progbar
#' @description Call \code{pbapply::\link[pbapply]{pbsapply}} 
#'              with nc default at number of cores available.
#'              Also, this works on Windows directly.
#'              Note this throws an error on unix systems, unlike \code{parallel::mclapply}
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
#' inp_chr_named <- list(first=1, second=2, third="3", fourth=4, fifth="5")
#' inp_num_named <- lapply(inp_chr_named, as.numeric)
#' inp_chr_none <- unname(inp_chr_named)
#' inp_num_none <- unname(inp_num_named)
#' if(FALSE){#intentional errors, don't run
#' par_sapply(inp_chr_named, log) # fails with name(s)
#' par_sapply(inp_num_named, log) # works, has names
#' par_sapply(inp_chr_none, log) # fails with index number (s)
#' par_sapply(inp_num_none, log) # no names, like in sapply
#' }
#' 
#' @param X              vector / list of values
#' @param FUN            function to be executed with each element of \code{X}.
#' @param nc             Integer: number of cores to be used in parallel.
#'                       DEFAULT: NULL (available cores)
#' @param pb             Show progress bar with remaining time and at the end runtime? 
#'                       DEFAULT: TRUE
#' @param simplify       Simplify output to vector/matrix if possible?
#'                       Note that simplify="array" is not implemented here.
#'                       DEFAULT: TRUE
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
  {
  output <- suppressWarnings(pbapply::pblapply(X=X, FUN=FUN, ..., cl=nc))
  } else
  {
# Slightly more work on windows: 
  cl <- parallel::makeCluster(nc)
  if(!is.null(export_objects)) parallel::clusterExport(cl, export_objects)
  output <- suppressWarnings(pbapply::pblapply(X=X, FUN=FUN, ..., cl=cl))
  parallel::stopCluster(cl)
  gc()
  }
failed <- sapply(output, inherits, "try-error")
if(any(failed)) 
  {
  of <- output[failed]
  on <- if(is.null(names(of))) paste("Element", which(failed)) else names(of)
  stop(paste(on, of, sep=": "))
  }
if(simplify) output <- simplify2array(output)
return(output)
}
