#' Create new filename if file already exists
#'
#' Check if files already exist and append \code{_1} or \code{_2}, etc to the filename if needed,
#' thereby giving useful messages.
#'
#' @name newFilename
#' @return newFilename returns the input with an added "_n" in the filename
#'         for each file that already existed.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016 + Jan 2017
#' @seealso \code{\link{file.exists}}
#' @keywords file
#' @importFrom tools file_ext file_path_sans_ext
#' @export
#' @examples
#'
#' fns <- c("data", "stupiddummy", "ExampleGraph.png", "berryFunctions.Rproj", 
#'          "README.md", "stupiddummy.txtdude", "DESCRIPTION", "test_devel.R")
#' newFilename(fns)
#' newFilename(fns, ignore=TRUE)
#' newFilename(fns, ignore=rep(0:1, each=4))
#' newFilename(fns, ntrunc=2)
#' newFilename("README.md")
#' newFilename("dummy", mid="") # no line break
#'
#' @param filename Char (vector): file name(s).
#' @param ignore   Logical (vector, recycled): Ignore file? DEFAULT: FALSE
#' @param pre,mid,end Char: strings to append after traceback / message / filenames. 
#'                 DEFAULT: "", "\\n  ", ""
#' @param quiet    Logical: Suppress messages about creating file(s)? DEFAULT: FALSE
#' @param ntrunc   Integer: Number of filenames printed in messages before they
#'                 get truncated with message "(and xx more)". DEFAULT: 3
#'
newFilename <- function(
filename,
ignore=FALSE,
pre="",
mid="\n  ",
end="",
quiet=FALSE,
ntrunc=3
)
{
ignore <- rep(ignore, length.out=length(filename))
output <- lapply(seq_along(filename), function(i)
  {
  f <- filename[i]
  if(ignore[i]) return(c(NA, f))
  #
  e2 <- tools::file_ext(f)
  if(e2!="") e2 <- paste0(".",e2)
  e1 <- tools::file_path_sans_ext(f)
  existed <- FALSE
  nr <- 1
  while(file.exists(f))
    {
    f <- paste0(e1,"_",nr,e2)
    nr <- nr + 1
    existed <- TRUE
    }
  return(c(existed, f))
  })
fnames  <- sapply(output, "[", 2)
existed <- sapply(output, "[", 1)
existed <- as.logical(existed)
if(!quiet)
  {
  # message names:
  n_e <- sum(existed, na.rm=TRUE) # number of existing files
  n_n <- sum(!is.na(existed)) # number of new files
  n_i <- sum(is.na(existed)) # number of ignored files
  n_o <- sum(file.exists(fnames[is.na(existed)])) # overwritten
  n_i <- n_i-n_o
  message(traceCall(1, "", ": "), pre,
          if(n_i>0) paste0("not checking ", n_i, " file", if(n_i>1)"s", if(n_o>0|n_n>0)", "),
          if(n_o>0) paste0( "overwriting ", n_o, " file", if(n_o>1)"s", if(n_n>0)", "),
          if(n_n>0) paste0(    "creating ", n_n, " file", if(n_n>1)"s"),
          if(n_e>0) paste0(" (",n_e," already existed for which '_n' is appended)"),
          ":", mid, truncMessage(fnames, ntrunc=ntrunc, prefix=""), end)
  }
fnames
}
