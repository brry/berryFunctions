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
#' fns <- c("dummy1", "dummy2.txt", "berryFunctions.Rproj",
#'          "README.md", "dummy2.dummy", "DESCRIPTION", "dummy4.R", "dummy5")
#' newFilename(fns)
#' newFilename(fns, ignore=TRUE)
#' newFilename(fns, ignore=rep(c(TRUE,FALSE), each=4) )
#' newFilename(fns, ignore=rep(c(TRUE,FALSE), each=4), tellignore=FALSE)
#' newFilename(fns, ntrunc=2)
#' newFilename(fns, overwrite=TRUE, ign=c(TRUE,TRUE,rep(FALSE,6)))
#' newFilename("README.md")
#' newFilename("dummy", mid=" ") # no line break
#' 
#' @param filename Char (vector): file name(s).
#' @param ignore   Logical (vector, recycled): Ignore file? DEFAULT: FALSE
#' @param overwrite Logical (vector, recycled): overwrite file? DEFAULT: FALSE
#' @param tellignore Logical: Message about ignored files? DEFAULT: TRUE
#' @param pre,mid,end Char: strings to append after traceback / message / filenames.
#'                 DEFAULT: "", "\\n  ", ""
#' @param quiet    Logical: Suppress messages about creating file(s)? DEFAULT: FALSE
#' @param ntrunc   Integer: Number of filenames printed in messages before they
#'                 get truncated with message "(and xx more)". DEFAULT: 3
#' 
newFilename <- function(
filename,
ignore=FALSE,
overwrite=FALSE,
tellignore=TRUE,
pre="",
mid="\n",
end="",
quiet=FALSE,
ntrunc=3
)
{
# change "./someFile" to actual location:
filename <- normalizePathCP(filename, winslash="/", mustWork=FALSE)
# check folder existence:
dirs <- unique(dirname(filename))
direxi <- file.exists(dirs)
l1 <- sum(!direxi)>1
if(any(!direxi)) stop(traceCall(1, "", ": "), "The following ",
                      if(l1)paste0(sum(!direxi)," "), "folder", if(l1)"s",
                      " do", if(!l1)"es", " not exist: ",
                      truncMessage(dirs[!direxi], ntrunc=ntrunc, prefix=""),
                      call.=FALSE)
# Actual code:
ignore <-    rep(ignore,    length.out=length(filename))
overwrite <- rep(overwrite, length.out=length(filename))
output <- lapply(seq_along(filename), function(i)
  {
  f <- filename[i]
  if(ignore[i])       return(c("ign", f))
  if(!file.exists(f)) return(c("new", f))
  if(overwrite[i])    return(c("ovw", f))
  # file name part 1 and 2
  p2 <- tools::file_ext(f)
  if(p2!="") p2 <- paste0(".",p2)
  p1 <- tools::file_path_sans_ext(f)
  nr <- 1
  while(file.exists(f))
    {
    f <- paste0(p1,"_",nr,p2)
    nr <- nr + 1
    }
  return(c("app", f))
  })
fnames <- mnames <- sapply(output, "[", 2)
status <- sapply(output, "[", 1)
if(!quiet)
  {
  # message names: number of files
  n_i <- sum(status=="ign") # ignored
  n_n <- sum(status=="new") # new
  n_o <- sum(status=="ovw") # overwritten
  n_a <- sum(status=="app") # _n appended
  if(!tellignore) {n_i <- 0; mnames <- mnames[!ignore]}
  n_ie<- sum(file.exists(filename[ignore]))
  nfiles <- function(n, name=FALSE) paste0(n, " file", if(name) "name", if(n>1)"s")
  if(n_i+n_n+n_o+n_a>0) message(traceCall(1, "", ": "), pre,
          if(n_i>0) paste0("not changing ", nfiles(n_i, name=TRUE),
                           " (",n_ie," exist",if(n_ie==1)"s",")"),
          if(n_i>0 & n_n+n_a+n_o > 0) ", ",
          if(n_n+n_a>0) paste0("creating ", nfiles(n_n+n_a)),
          if(n_a>0) paste0(" (",n_a," already existed for which '_n' is appended)"),
          if(n_i+n_n+n_a > 0 & n_o>0) ", ",
          if(n_o>0) paste0("overwriting ", nfiles(n_o)),
          ":", mid, truncMessage(mnames, ntrunc=ntrunc, prefix="", midfix="", sep="\n"), end)
  }
fnames
}
