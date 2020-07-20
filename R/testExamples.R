#' @title Test examples in a package
#' @description Test all examples in a package
#' @return Logical indicating successful tests
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Mar 2019
#' @seealso The evaluate package
#' @keywords package
#' @importFrom tools Rd2ex
#' @importFrom grDevices dev.off pdf
#' @importFrom utils read.table
#' @export
#' @examples
#' # testExamples(selection=1:10)
#'
#' @param path           Path to package. For internal function \code{testExample}, 
#'                       path to a single Rd file.
#'                       DEFAULT: \code{\link{packagePath}(".")}
#' @param commentDontrun Logical. Should \\dontrun sections be excluded?
#'                       DEFAULT: FALSE
#' @param selection      Optional: selection of files, e.g 1:10. DEFAULT: NULL
#' @param logfolder      Directory where to store the logfiles. Created if not 
#'                       existing. DEFAULT: "ExampleTestLogs"
#' @param elogfile       File to log errors in. (Appended to existing text). 
#'                       DEFAULT: "errors.txt"
#' @param wlogfile       File to log warnings and messages in. (Appended to existing text). 
#'                       DEFAULT: "warnings.txt"
#' @param tlogfile       File in which to write computing times. 
#'                       DEFAULT: "times.txt"
#' @param plotfile       File to log warnings and messages in. (Appended to existing text). 
#'                       DEFAULT: "plots.pdf"
#' @param tellcurrentfile Logical: At the beginning of each file, message the
#'                       name and current time in the console?
#' @param telldocument   Message reminder to run \code{devtools::document()}? 
#'                       DEFAULT: TRUE
#' @param \dots          Further arguments passed to internal function \code{testExample}
#'                       and from there to \code{tools::\link{Rd2ex}}
#'
testExamples <- function(
path=packagePath("."),
commentDontrun=FALSE,
selection=NULL,
logfolder="ExampleTestLogs",
elogfile="errors.txt",
wlogfile="warnings.txt",
tlogfile="times.txt",
plotfile="plots.pdf",
tellcurrentfile=TRUE,
telldocument=TRUE,
...
)
{
if(telldocument) message("Make sure you have run devtools::document() recently!")
if(!file.exists(logfolder)) dir.create(logfolder)
elogfile <- normalizePathCP(paste0(sub("/$","",logfolder),"/",elogfile))
wlogfile <- normalizePathCP(paste0(sub("/$","",logfolder),"/",wlogfile))
tlogfile <- normalizePathCP(paste0(sub("/$","",logfolder),"/",tlogfile))
plotfile <- normalizePathCP(paste0(sub("/$","",logfolder),"/",plotfile))
owd <- setwd(logfolder) # examples may write to disc at relative path
on.exit(setwd(owd), add=TRUE)
cat("", file=tlogfile) # clean runtime logfile, otherwise extimes will have too many entries
# Suppress progbars in logfiles:
if(requireNamespace("pbapply", quietly=TRUE))
  {
  pbtype <- pbapply::pboptions(type="none")
  on.exit(pbapply::pboptions(type=pbtype$type), add=TRUE)
  }
# Get the man pages in the package:
manfiles <- dir(paste0(path,"/man"), full.names=TRUE)
if(!is.null(selection)) manfiles <- manfiles[selection]
pdf(plotfile)
on.exit(dev.off(), add=TRUE)
# call the function doing the actual work (testExample):
mantests <- sapply(manfiles, testExample, commentDontrun=commentDontrun, 
                   elogfile=elogfile, wlogfile=wlogfile, tlogfile=tlogfile, 
                   plotfile=NULL, tellcurrentfile=tellcurrentfile, ...)
# Tell about long example calculation times:
extimes <- read.table(tlogfile, sep=";", stringsAsFactors=FALSE)
extimes <- sortDF(extimes, 2)
message("Total example run time: ", round(sum(extimes[,2])/60,1), " minutes.")
over <- paste0(basename(extimes[,3]), "(", round(extimes[,2]), ")")[extimes[,2]>5]
over <- sub(".Rd", "", over, fixed=TRUE)
message("Time >5 secs in ",length(over)," Rd files: ", toString(over))
# Tell about errors:
manfailed <- manfiles[mantests>0]
failed <- any(mantests>0)
msg <- if(failed) paste0("\nErrors in ", sum(mantests>0), " out of ", 
       length(manfiles), " documentation examples in ") else "No errors in "
msg <- paste0(msg, path, if(failed) ".\nErrors, w" else "\nW",
        "arnings / messages / cats and computing times are logged in \n'",
        if(failed) elogfile, if(failed) "', '", wlogfile, "' and '", tlogfile, "'.")
if(failed) msg <- paste0(msg, "\nFailures occurred in: ", 
                         toString(basename(manfailed)))
message(msg)
}



 
testExample <- function(
path,
commentDontrun=FALSE,
elogfile="Examples_errors.txt",
wlogfile="Examples_warnings.txt",
tlogfile="Examples_times.txt",
plotfile="Examples_plots.pdf",
tellcurrentfile=TRUE,
...
)
{
if(tellcurrentfile) message(format(Sys.time(), "%T"), " Testing examples ",
             if(!commentDontrun) "(including \\dontrun sections) ","in: ", path)

# to get sink and tryCatch to work together:
# https://stackoverflow.com/questions/25320381/sink-doesnt-work-in-trycatch-block
oop <- options(warn=1)
on.exit(options(oop), add=TRUE)

# get code from example sections:
ex_path <- file.path(tempdir(), paste0(basename(path), "_test.R"))
tools::Rd2ex(path, ex_path, commentDontrun=FALSE, ...)
if(!file.exists(ex_path)) {message("No examples were parsed in ", path); return(0)}
rd_ex_parsed <- parse(file=ex_path)

# Environment in which to evaluate the code:
env <- new.env(parent=globalenv())
env$NumberOfErrorsInExampleTest <- 0

# Function to nicely log errors to logfile:
efun <- function(e)
 {
 catf <- function(...) cat(..., file=elogfile, append=TRUE)
 catf("\n---------------\n", path, " -- ", as.character(Sys.time()),"\n\n")
 catf(deparse(rd_ex_parsed[[repi]]),"\n-", as.character(e), "\n")
 env$NumberOfErrorsInExampleTest <- env$NumberOfErrorsInExampleTest + 1
 }

# optional plot output:
if(!is.null(plotfile))
 {
 pdf(plotfile)
 on.exit(dev.off(), add=TRUE)
 }

# Capture warnings, messages and cat-output:
wlog_tempfile <- tempfile("Warn_log", fileext=".txt")
wlog <- file(wlog_tempfile, open="wt")
sink(wlog, type="output" , append=TRUE)
sink(wlog, type="message", append=TRUE)

# Actually evaluate the expressions one by one:
starting_time <- Sys.time()
message("\n---------------\n", path, " -- ", as.character(Sys.time()),"\n")
for(repi in seq_along(rd_ex_parsed)) tryCatch(eval(rd_ex_parsed[[repi]], envir=env), 
                                            error=efun)
computing_time <- difftime(Sys.time(), starting_time, units="secs")

# Close capturing:
sink(type="message")
sink()
close(wlog)                          #   closeAllConnections()

# Copy warnings et al to warnings logfile:
cat(readLines(wlog_tempfile, warn=FALSE), sep="\n", file=wlogfile, append=TRUE)
# Write computing time
cat(paste(starting_time, ";", round0(computing_time, 4, pre=3), ";", path), 
    "\n", file=tlogfile, append=TRUE)
# Output: number of errors in code:
env$NumberOfErrorsInExampleTest
}


# Alternative to  parse -> tryCatch(eval):
# evres <- evaluate::evaluate(file(ex_path), include_timing=F)
# ii <- grepl("error|warning", sapply(evres, class))   
# evres[ii]
# evaluate::replay(evres[ii])
