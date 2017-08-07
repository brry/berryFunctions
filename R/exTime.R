#' Time to run examples
#' 
#' Time the execution of examples. Useful in package development to identify functions taking much time.
#' 
#' @return Time used as per \code{\link{system.time}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{example}}, \code{\link{system.time}}
#' @keywords documentation utilities
#' @importFrom utils capture.output
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics plot text
#' @export
#' @examples
#' exTime("plot")
#' exTime("yearSample", quiet=TRUE)
#' exTime(yearSample) # does NOT work, gives NULL and warning
#' exTime("yearSample", elapsed=TRUE, quiet=TRUE)
#' exTime("addFade", elapsed=TRUE, quiet=TRUE, run.dontrun=TRUE, run.donttest=TRUE)
#' 
#' ## this takes quite some time if done for all functions in a package:
#' \dontrun{
#' times <- exTime(package="rdwd")
#' as.matrix(sort(times))
#' system2("open", tempdir()) # to view the pdf graphics created by exTime
#' 
#' # times <- exTime(package="rdwd", run.dontrun=FALSE)
#' }
#' 
#' @param topic     Character string: the online \code{\link{help}} topic
#'                  of which the examples should be run.
#' @param package   Charstring: installed and loaded package from which all examples should be run.
#' @param echo      Show the R input when sourcing? DEFAULT: FALSE
#' @param elapsed   Return *only* the third element (total elapsed time)? DEFAULT: FALSE
#' @param imagefile Reroute graphics to \code{\link{pdf}} device?
#'                  Will \code{\link{message}} the \code{\link{tempfile}}
#'                  location if quiet=FALSE.  DEFAULT: TRUE
#' @param quiet     Suppress warnings with both \code{\link{suppressWarnings}} and
#'                  \code{\link{suppressMessages}},
#'                  also \code{\link{capture.output}} for str and cat results
#'                  as well as setting \code{pboptions(type="none")} if \code{pbapply} is available.
#' @param \dots     Further arguments to \code{\link{example}}, especially
#'                  \code{run.dontrun}, \code{run.donttest} and \code{package},
#'                  but NOT \code{character.only} and \code{ask}
#' 
exTime <- function(
topic=NA,
package=NA,
echo=FALSE,
elapsed=FALSE,
imagefile=TRUE,
quiet=FALSE,
...
)
{
#
# Reroute graphics:
if(imagefile)
  {
  filename <- paste0(tempdir(), "/exTime ", gsub(":","-",as.character(Sys.time())), ".pdf")
  pdf(filename)
  on.exit(dev.off(), add=TRUE)
  if(!quiet) on.exit(message(filename), add=TRUE)
  }
exTimeSingle <- function(topic, echo, elapsed, imagefile, quiet, catdot=FALSE, ...){
if(!is.character(topic)) {warning("topic must be a character string"); return()}
if(topic=="exTime") return()
if(catdot) cat(".")
# silence progress bars if applicable
if(requireNamespace("pbapply", quietly=TRUE)) if(quiet)
  {
  opo <- pbapply::pboptions(type="none")
  on.exit(pbapply::pboptions(opo), add=TRUE)
  }
# write expression only once, then conditionally suppress warnings:
topic <- topic # evaluate promise
expr <- substitute(
result <- system.time(example(topic=topic, character.only=TRUE, ask=FALSE, echo=echo, ...))
)
# Actually time the examples:
if(quiet) dummy <- capture.output(suppressWarnings(suppressMessages( eval(expr) )))

     else eval(expr)
if(elapsed) result <- as.numeric(result[3])
# Output the resulting time:
# graphical output
if(imagefile)
  {
  plot(1, type="n", ann=FALSE, axes=FALSE) # outro graph
  moreargs <- unlist(list(...))
  catformat <- function(x) toString(paste(names(x), x, sep="="))
  text(1,1, paste0("berryFunctions::exTime\n\n", topic, "\n\nElapsed time  [sec]:\n\n", catformat(round(result,5)),
                   "\n\n", catformat(moreargs)))
  }
# function output
result
}
# actually run stuff
if(!is.na(package))
  {
  if(missing(quiet)) quiet <- TRUE
  if(missing(elapsed)) elapsed <- TRUE
  funs <- ls(paste0("package:",package))
  cat("processing ", length(funs), " functions.\n")
  sapply(funs, exTimeSingle, catdot=TRUE, echo=echo, elapsed=elapsed, imagefile=imagefile, quiet=quiet, ...)
  } else
exTimeSingle(topic=topic, echo=echo, elapsed=elapsed, imagefile=imagefile, quiet=quiet, ...)
}
