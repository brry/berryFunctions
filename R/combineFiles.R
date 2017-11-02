#' Combine Textfiles into one
#' 
#' Combine several textfiles into one, regardless of their content.
#' 
#' @return None, but prints number of files combined and output file name.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2012, Dec 2014, Jul 2015
#' @seealso \code{\link{compareFiles}}, and the functions used internally here, namely: \code{\link{paste}}, \code{\link{scan}}, \code{\link{write}}.
#' @keywords IO file character
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @examples
#' 
#' ## These are skipped by rcmd check (writing to external places is not allowed)
#' \dontrun{
#' cat("This is Sparta.\nKicking your face.", file="BujakashaBerry1.txt")
#' cat("Chuck Norris will roundhousekick you.", file="BujakashaBerry2.txt")
#' combineFiles(inFiles=paste0("BujakashaBerry", 1:2, ".txt"),
#'              outFile="BujakashaBerry3.txt")
#' file.show("BujakashaBerry3.txt")
#' unlink(paste0("BujakashaBerry", 1:3, ".txt"))
#' }
#' 
#' @param inFiles   vector with names of input files, as can be read with 
#'                  \code{\link{scan}}. DEFAULT: dir()
#' @param outFile   Character string: name of the file to be created. Passed to
#'                  \code{\link{newFilename}}. DEFAULT: "combined_Textfiles.txt"
#' @param overwrite Logical: overwrite outFile? DEFAULT: FALSE 
#' @param sep       Character string: Separation between content of 
#'                  each file and the following. DEFAULT: NULL, with which it uses 
#'                  an empty line, two lines with dashes, and another line break.
#' @param names     Should File names be included after sep? DEFAULT: TRUE
#' @param selection Index of rows that should be written. Can refer to each file 
#'                  separately, e.g. \code{substr(inFile_i,1,1)=="#"}.
#'                  DEFAULT: all lines
#' @param progbar   Should a progress bar be drawn? Useful if you combine many 
#'                  large files. DEFAULT: !quiet, i.e. TRUE
#' @param quiet     Suppress message about number of files combined? DEFAULT: FALSE
#' @param \dots     Arguments passed to \code{\link{scan}}, but not one of: 
#'                  \code{file, what, blank.lines.skip, sep, quiet}.
#' 
combineFiles <- function(
   inFiles   = dir(),
   outFile   = "combined_Textfiles.txt",
   overwrite = FALSE,
   sep       = NULL,
   names     = TRUE,
   selection = NULL,
   progbar   = !quiet,
   quiet     = FALSE,
   ...)
{
# Function start
inFiles <- inFiles # execute before outFile is added
# Default sep:
if(is.null(sep)) sep <- "\n-------------------------------------------------------
-------------------------------------------------------\n"
# File to write to:
File <- newFilename(outFile, overwrite=overwrite, quiet=TRUE)
write("", file=File)
# Meta information if wanted
if(names)
 {
 write(paste(length(inFiles), "Files in", getwd()), file=File, append=TRUE)
 write("Combined together with berryFunctions::combineFiles", file=File, append=TRUE)
 write(as.character(Sys.time()), file=File, append=TRUE)
 write(sep, file=File, append=TRUE)
 }
# Progressbar, if wanted:
if(progbar) pb <- txtProgressBar(max=length(inFiles), style=3)
# The actual action
for(i in 1:length(inFiles))
   {
   # Read file:
   inFile_i <- scan(file=inFiles[i], what="char",
                    blank.lines.skip=FALSE, sep="\n", quiet=TRUE, ...)
   # Write filename if wanted:
   if(names) write(paste(inFiles[i], "\n"), file=File, append=TRUE)
   # selection of lines to write to output
   selection2 <- eval(substitute(selection), envir=environment())
   if(is.null(selection2)) selection2 <- 1:length(inFile_i)
   # write selection to output
   write(inFile_i[selection2], file=File, append=TRUE)
   # Write separation:
   write(sep, file=File, append=TRUE)
   # Update progres bar:
   if(progbar) setTxtProgressBar(pb, i)
   } # End of for-Loop
if(progbar) close(pb)
if(!quiet) message(i, " files combined to ", File)
} # End of function
