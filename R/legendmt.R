#' legend with multiline title
#' 
#' Draw a legend with title spanning several lines (i.e. with line breaks).
#' Note that this is in development and not all inputs ar correctly vectorized yet.
#' 
#' @return \code{\link{legend}} output
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr 2017
#' @seealso \code{\link{legend}}
#' @keywords aplot
#' @importFrom graphics legend par
#' @export
#' @examples
#' plot(1:10)
#'   legend("topleft", letters[1:4], col=1:4, pch=1, title="very long title to be split up")
#' legendmt("topleft", letters[1:4], col=1:4, pch=1, title="very long title\nnow splat up")
#' 
#' # Alternative:
#' plot(1:10)
#' legend("topleft", "very long title to be split up")
#' legend("topleft", letters[1:4], col=1:4, pch=1, inset=c(0,0.09) )
#' 
#' @param x,y,legend Arguments as in \code{\link{legend}}
#' @param title      Character with linebreaks or vector of charstrings.
#' @param x.intersp,fill,col,border,lty,lwd,pch Arguments as in \code{\link{legend}}
#' @param \dots      Further arguments passed to \code{\link{legend}}.
#'                   If vectorized, please remember to prepend NAs or whatever.
#' 
legendmt <- function(
x,
y=NULL,
legend,
title,
x.intersp=1,
fill=NA,
col=par("col"),
border=NA,
lty=NA,
lwd=NA,
pch=NA,
...
)
{
## the 2nd arg may really be `legend'
if(missing(legend) && !missing(y) && (is.character(y) || is.expression(y)))
 {
	legend <- y
	y <- NULL
 }
# title must be a vector of strings:
title <- unlist(strsplit(title,"\n"))
# prepend values:
lt <- length(title)
ll <- length(legend)
nas <- rep_len(NA,lt)
x.intersp <- c(rep_len(0,lt), rep_len(x.intersp,ll)  )
fill   <- c(nas, rep_len(fill,  ll) )
col    <- c(nas, rep_len(col,   ll) )
border <- c(nas, rep_len(border,ll) )
lty    <- c(nas, rep_len(lty,   ll) )
lwd    <- c(nas, rep_len(lwd,   ll) )
pch    <- c(nas, rep_len(pch,   ll) )
# legend
legend <- c(title,legend)
# actual drawing:
graphics::legend(x,y,legend, x.intersp=x.intersp, fill=fill, col=col,
                 border=border, lty=lty, lwd=lwd, pch=pch, ...)
}
