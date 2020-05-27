#' Create pdf and png graph
#' 
#' Create both a \code{\link{pdf}} and a \code{\link{png}} file with a graph,
#' with custom size default values.\cr
#' \code{pdfpng} tries to open the PDF file (through \code{\link{openPDF}}) 
#' with SumatraPDF viewer, which does not lock files against being edited. \cr
#' See \code{\link{sumatraInitialize}} for nice Sumatra default settings.
#' @return file paths, invisible
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, March 2017
#' @seealso \code{\link{pdf}}, \code{\link{png}}
#' @keywords file
#' @importFrom grDevices pdf png dev.off
#' @importFrom stats runif
#' @export
#' @examples
#' 
#' \dontrun{ # excluded from CRAN checks, file opening not wanted
#' pdfpng(   plot(rnorm(500), type="l")   , file="dummyplot", png=FALSE)
#' }
#' 
#' pdfpng({par(bg=8, las=1); plot(cumsum(rnorm(500)), type="l")},
#'        file="dummyplot", res=100, open=FALSE)
#' pdfpng({par(bg=8, las=1); plot(cumsum(rnorm(500)), type="l")},
#'        file="dummyplot", overwrite=c(TRUE,FALSE), open=FALSE)
#' 
#' # Nesting of functions is possible:
#' a <- list( cumsum(rnorm(2000)), cumsum(rnorm(20)) )
#' pdfpng(plot(a[[1]]), file="dummyplot", overwrite=TRUE, open=FALSE)
#' bfun <- function(b) pdfpng(plot(b,type="l"), file="dummyplot", 
#'                            overwrite=TRUE, open=FALSE)
#' cfun <- function(c) bfun(c)
#' bfun(a[[1]])
#' sapply(a, function(d) cfun(d))
#' 
#' 
#' pdfpng(plot(-10:100, log="y"), "dummyplot",overwr=TRUE,png=FALSE, open=FALSE)
#' pdfpng({plot(1); plot(dummyobject)}, "dummyplot", overwrite=TRUE, 
#'        png=FALSE, open=FALSE)
#' 
#' 
#' unlink("dummyplot.pdf") ; unlink("dummyplot.png") ; unlink("dummyplot_1.png")
#' 
#' @param expr         Expression creating the plot, can be included in curly braces.
#' @param file         Character: Filename without pdf/png extension.
#'                     Unless overwrite=TRUE, files will not be overwritten, but
#'                     "_1" will be appended instead, see \code{\link{newFilename}}.
#'                     If \code{expr} creates several plots, use file="fname\%02d",
#'                     otherwise the png will only contain the last figure.
#'                     Note: this overwrites files as the \% notation is not
#'                     captured by newFilename. You may also have to run dev.off().
#' @param pdf          Logical: Create pdf? DEFAULT: TRUE
#' @param png          Logical: Create png? DEFAULT: TRUE
#' @param overwrite    Logical: Overwrite existing \code{file}? Can be a vector
#'                     for pdf and png separately.
#'                     DEFAULT: FALSE (_n appended in filename)
#' @param open         Logical: open file(s) after creation using 
#'                     \code{\link{openPDF}} and \code{\link{openFile}}? 
#'                     DEFAULT: TRUE 
#' @param quiet        Logical: suppress file creation messages and
#'                     expr execution error tracing? DEFAULT: FALSE
#' @param tracewarnmes Logical: trace warnings and messages in expr execution?
#'                     Errors are always traced. Default: !quiet
#' @param filargs      List of other arguments passed to \code{\link{newFilename}}.
#'                     DEFAULT: NULL
#' @param width,height Graph dimensions. DEFAULT: 7x5 inches
#' @param units,res    Graph quality arguments passed only to \code{\link{png}}.
#'                     DEFAULT: inches ("in"), 500 ppi
#' @param seed         Seed passed to \code{\link{set.seed}} before each call.
#'                     DEFAULT: runif(1,-1e9,1e9)
#' @param envlevel     Environment level passed to \code{\link{eval.parent}}.
#'                     Never needs to be changed, as far as I can tell. DEFAULT: 1
#' @param pdfargs      List of arguments only passed to \code{\link{pdf}}.
#' @param pngargs      List of arguments only passed to \code{\link{png}}.
#' @param \dots        Further arguments passed to both \code{\link{pdf}} and \code{\link{png}}
#' 
pdfpng <- function(
 expr,
 file,
 pdf=TRUE,
 png=TRUE,
 overwrite=FALSE,
 open=TRUE,
 quiet=FALSE,
 tracewarnmes=!quiet,
 filargs=NULL,
 width=7,
 height=5,
 units="in",
 res=500,
 seed=runif(1,-1e9,1e9),
 envlevel=1,
 pdfargs=NULL,
 pngargs=NULL,
 ...
 )
{
# input checks
if(!is.logical(pdf)) stop("pdf argument must be logical (T/F), not '", class(pdf),"'.")
if(!is.logical(png)) stop("pdf argument must be logical (T/F), not '", class(png),"'.")
if(!pdf & !png) {warning("pdf and png both FALSE, not saving plot."); return(expr)}
# File names
fig <- normalizePath(file, winslash="/", mustWork=FALSE)
fig <- paste0(fig, c(".pdf",".png"))
# do not overwrite existing files
fig <- do.call(newFilename, owa(
              list(filename=fig[c(pdf,png)], ignore=overwrite, quiet=quiet), filargs))
fig <- rep(fig, length.out=2) # in case pdf=FALSE, fig[2] should be the png path
# Arguments
dots <- list(...)
seed <- seed # force evaluation
# Actually create graphics:
if(pdf)
{
  do.call(grDevices::pdf, owa(
    c(list(file=fig[1], width=width, height=height), dots), pdfargs))
  set.seed(seed)
  tryStack( eval.parent(substitute(expr), envlevel), silent=quiet, warn=tracewarnmes,
            skip=c("tryStack(eval.parent(substitute(expr), envlevel), silent = quiet, ",
                   "eval(expr, p)"))
  dev.off()
}
if(png)
{
  do.call(grDevices::png, owa(
    c(list(file=fig[2], width=width, height=height, units=units, res=res), dots), pngargs))
  set.seed(seed)
  tryStack( eval.parent(substitute(expr), envlevel), silent=quiet, warn=tracewarnmes,
            skip=c("tryStack(eval.parent(substitute(expr), envlevel), silent = quiet, ",
                   "eval(expr, p)"))
  dev.off()
}
if(open) { if(pdf) openPDF(fig[1]) ; if(png) openFile(fig[2])  }
return(invisible(fig))
}
