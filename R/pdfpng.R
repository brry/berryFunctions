#' Create pdf and png graph
#'
#' Create both a \code{\link{pdf}} and a \code{\link{png}} file with a graph,
#' with custom size default values.
#' To iteratively create pdfs without closing and reopening the pdf viewer, you 
#' might want to use e.g. Sumatra, which comes with Rstudio. It can be found e.g. in
#' C:/Program Files/RStudio/bin/sumatra
#'
#' @return Nothing
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, March 2017
#' @seealso \code{\link{pdf}}, \code{\link{png}}
#' @keywords file
#' @importFrom grDevices pdf png dev.off
#' @importFrom stats runif
#' @export
#' @examples
#'
#' pdfpng({par(bg=8, las=1); plot(cumsum(rnorm(500)), type="l")}, 
#'        file="dummyplot", res=100)
#' pdfpng({par(bg=8, las=1); plot(cumsum(rnorm(500)), type="l")}, 
#'        file="dummyplot", overwrite=c(TRUE,FALSE))
#'
#' # Nesting of functions is possible:
#' a <- list( cumsum(rnorm(2000)), cumsum(rnorm(20)) )
#' pdfpng(plot(a[[1]]), file="dummyplot", overwrite=TRUE)
#' bfun <- function(b) pdfpng(plot(b,type="l"), file="dummyplot", overwrite=TRUE)
#' cfun <- function(c) bfun(c)
#' bfun(a[[1]])   
#' sapply(a, function(d) cfun(d))    
#'
#'        
#' unlink("dummyplot.pdf") ; unlink("dummyplot.png") ; unlink("dummyplot_1.png")
#'
#' @param expr         Expression creating the plot, can be included in curly braces.
#' @param file         Character: Filename without pdf/png extension. 
#'                     Unless overwrite=TRUE, files will not be overwritten, but
#'                     "_1" will be appended instead, see \code{\link{newFilename}}.
#' @param pdf          Logical: Create pdf? DEFAULT: TRUE
#' @param png          Logical: Create png? DEFAULT: TRUE
#' @param overwrite    Logical: Overwrite existing \code{file}? Can be a vector
#'                     for pdf and png separately.
#'                     DEFAULT: FALSE (_n appended in filename)
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
fig <- newFilename(fig[c(pdf,png)], ignore=overwrite)
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
  eval.parent(substitute(expr), envlevel)
  dev.off()
}
if(png) 
{ 
  do.call(grDevices::png, owa(
    c(list(file=fig[2], width=width, height=height, units=units, res=res), dots), pngargs))
  set.seed(seed)
  eval.parent(substitute(expr), envlevel)
  dev.off()
} 

}
