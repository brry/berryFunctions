#' scalebar for OSM plots
#'
#' Add a scalebar to default OpenStreetMap plots
#'
#' @details This is a starting version and might be expanded heavily in customization options
#'
#' @return invisible: coordinates of scalebar and label
#' @section Warning: Has not been extensively tested yet, please report bugs.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2016
#' @seealso \code{\link{help}}, \code{\link{help}}
#' @keywords aplot
#' @export
#' @examples
#'
#' \dontrun{## Not run because of OpenStreetMap dependency
#' library("OpenStreetMap")
#' d <- data.frame(lon=c(12.95, 12.98, 13.22, 13.11), lat=c(52.40,52.52, 52.36, 52.45))
#' bbox <- c(extendrange(d$lon), extendrange(d$lat))
#' map <- openmap(bbox[c(4,1)], bbox[c(3,2)], type="osm")
#' par(mar=c(2,2,3,0))
#' plot(map, removeMargin=FALSE)
#' points(projectMercator(d$lat,d$lon), pch=3, lwd=3)
#' coord <- scaleBarOSM()  ; coord
#' scaleBarOSM(0.3, 0.05, unit="m")
#' scaleBarOSM(0.3, 0.05, unit="m")
#' scaleBarOSM(0.12, 0.28, abslen=19000)
#' }
#'
#' @param x,y Relative position of left end of scalebar. DEFAULT: 0.1, 0.9
#' @param length Approximate relative length of bar. DEFAULT: 0.1
#' @param abslen Absolute length in \code{unit}s. DEFAULT: NA (computed internally from length)
#' @param unit Unit to compute and label. Currently, only "km" is possible. DEFAULT: "km"
#' @param field,fill,adj Arguments passed to \code{\link{textField}}
#' @param textargs List of further arguments passed to \code{\link{textField}}
#'                 like cex, col, etc. DEFAULT: NULL
#' @param lwd,lend Line width and end style passed to \code{\link{segments}}. DEFAULT: 5,1
#' @param \dots Further arguments passed to \code{\link{segments}} like col
#'
scaleBarOSM <- function(
x=0.1,
y=0.9,
length=0.2,
abslen=NA,
unit="km",
field="rect",
fill=NA,
adj=c(0.5, 1.5),
textargs=NULL,
lwd=5,
lend=1,
...
)
{
warning("Comparison measurements in googleEarth indicate this is way off!")
# input checks:
x <- x[1]; y <- y[1]
if(x<0) stop("x must be larger than 0, not ", x)
if(y<0) stop("y must be larger than 0, not ", y)
if(x>1) stop("x must be lesser than 1, not ", x)
if(y>1) stop("y must be lesser than 1, not ", y)
# factor:
f <- if(unit=="m") 1 else
     if(unit=="km") 1000 else
     stop("unit '", unit,"' not (yet) supported.")
# coordinate range:
r <- par("usr")
# get absolute coordinates:
if(is.na(abslen)) abslen <- pretty(diff(r[1:2])/f*length)[2]*f
x <- r[1]+x*diff(r[1:2])
y <- r[3]+y*diff(r[3:4])
# draw segment:
segments(x0=x, x1=x+abslen, y0=y, lwd=lwd, lend=lend, ...)
# label scale bar:
xl <- x+0.5*abslen
do.call(textField, owa(list(x=xl, y=y, labels=paste(abslen/f, unit), field=field,
                            fill=fill, adj=adj), textargs))
# return absolute coordinates
return(invisible(c(x=x, y=y, abslen=abslen, label=xl)))
}
