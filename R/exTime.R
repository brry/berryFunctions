#' Time to run examples
#'
#' Time the execution of examples. Useful in package development to identify functions takeing much time.
#'
#' @return Time used as per \code{\link{system.time}}
#' @section Warning: warningMayBeRemoved
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{example}}, \code{\link{system.time}}
#' @keywords documentation utilities
#' @export
#' @examples
#' exTime("yearSample")
#' exTime("yearSample", quiet=TRUE)
#' exTime(yearSample) # does NOT work, gives NULL and warning
#' exTime("yearSample", elapsed=TRUE, quiet=TRUE)

#exTime("addFade", elapsed=TRUE, quiet=TRUE, run.dontrun=TRUE, run.donttest=TRUE)

#'
#' ## this takes quite some time if done for all functions in the package:
#' \dontrun{
#' fn <- ls("package:berryFunctions")[1:7]
#' ft <- rep(NA,length(fn))  ; names(ft) <- fn
#' for(f in fn)  ft[f] <- exTime(f, quiet=TRUE, elapsed=TRUE, run.dontrun=FALSE)
#' as.matrix(sort(ft))
#' system2("open", tempdir()) # to view the pdf graphics created by exTime
#' }
#'
#' @param topic Character string: the online \code{\link{help}} topic the examples of which should be run
#' @param echo Show the R input when sourcing? DEFAULT: FALSE
#' @param elapsed Return *only* the third element (total elapsed time)? DEFAULT: FALSE
#' @param imagefile Reroute graphics to \code{\link{pdf}} device? Will \code{\link{message}} the \code{\link{tempfile}} location if quiet=FALSE. DEFAULT: TRUE
#' @param quiet Suppress warnings with both \code{\link{suppressWarnings}} and \code{\link{suppressMessages}}, also \code{\link{capture.output}} for str and cat results as well as setting \code{pboptions(type="none")} if \code{pbapply} is available.
#' @param \dots Further arguments to \code{\link{example}}, especially \code{run.dontrun}, \code{run.donttest} and \code{package}, but NOT \code{character.only} and \code{ask}
#'
exTime <- function(
topic,
echo=FALSE,
elapsed=FALSE,
imagefile=TRUE,
quiet=FALSE,
...
)
{
if(!is.character(topic)) {warning("topic must be a character string"); return()}
#top2 <- deparse(substitute(topic))
#if(!is.character(topic)) topic <- top2
#
# silence progress bars if applicable
if(requireNamespace("pbapply", quietly=TRUE)) if(quiet)
  {
  opo <- pbapply::pboptions(type="none")
  on.exit(pbapply::pboptions(opo), add=TRUE)
  }
# write expression only once, then conditionally suppress warnings:
expr <- substitute(
result <- system.time(example(topic=topic, character.only=TRUE, ask=FALSE, echo=echo, ...))
)
# Reroute graphics:
if(imagefile)
  {
  filename <- tempfile(paste0("exTime_", topic, "_"), fileext=".pdf")
  pdf(filename)
  on.exit(dev.off(), add=TRUE)
  plot(1, type="n", ann=FALSE, axes=FALSE) # intro graph to avoid empty pdfs
  text(1,1, paste("berryFunctions::exTime output", Sys.time(), topic, sep="\n\n"))
  if(!quiet) on.exit(message(filename), add=TRUE)
  }
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
  catformat <- function(x) pastec(paste(names(x), x, sep="="))
  text(1,1, paste0(topic, " finished. Elapsed time  [sec]:\n\n", catformat(result),
                   "\n\n", catformat(moreargs)))
  }
# function output
result
}
