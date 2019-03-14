#' @title Rectangles with rounded corners
#' @description Draw \link{rect}angles with rounded corners via \code{\link{polygon}}
#' @return Final coordinates, invisible
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2017
#' @seealso \code{\link{textField}}
#' @keywords aplot
#' @export
#' @importFrom grDevices dev.size
#' @importFrom graphics par points polygon
#' @examples
#' plot(1:10)  ;  rect(4,2,7,8, border=8)
#' roundedRect(4,2,7,8, rounding=0.1)
#' roundedRect(4,2,7,8, rounding=0.25) # default
#' roundedRect(4,2,7,8, rounding=0.5)
#' roundedRect(4,2,7,8, rounding=-0.1, border="red")
#' roundedRect(4,2,7,8, rounding=1.1, border="blue")
#' roundedRect(2,2,8,4, rounding=0.5) # in long boxes, 0.5 is max
#' roundedRect(2,2,8,4, rounding=0.5, bothsame=FALSE, corfactor=1, border=3)
#' 
#' 
#' plot(1:10)  ;  rect(4,2,7,8, border=8)
#' roundedRect(4,2,7,8, corners=c(2,4))
#' 
#' plot(1:10, asp=1)  ;  rect(4,2,7,8, border=8)
#' roundedRect(4,2,7,8)
#' roundedRect(4,2,7,8, aspcorrect=FALSE, border="red") # results depend on asp
#' 
#' plot(1:10, asp=1.5)  ;  rect(4,2,7,8, border=8)
#' roundedRect(4,2,7,8)
#' roundedRect(4,2,7,8, aspcorrect=FALSE, border="red") # results depend on asp
#' 
#' plot(1:10, asp=1)  ;  rect(4,2,7,8, border=8)
#' roundedRect(4,2,7,8) # difference only visible if rect is clearly not a square:
#' roundedRect(4,2,7,8, bothsame=FALSE, border="red")
#' roundedRect(4,2,7,8, bothsame=FALSE, aspcorrect=TRUE, border="blue")
#' 
#' \dontrun{ # aspect correction factor determination
#' rrtest <- function(...) roundedRect(10,0.5, 35,15, border=2, factorpoints=TRUE)
#' pdfpng({plot(1:40                     ); rrtest();
#'         plot(1:40, ylim=c(0,15)       ); rrtest();
#'         plot(1:40, ylim=c(0,15), asp=1); rrtest();
#'         roundedRect(2,0, 8,15, factorpoints=TRUE);
#'         roundedRect(15,10, 25,16, npoints=200)}, 
#'        file="dummytest", png=F, overwrite=T)
#' }
#' 
#' @param xleft,ybottom,xright,ytop Single numbers with the outer end locations
#'                    of the rectangle.
#' @param rounding    Proportion of the box to round. Recommended to be between
#'                    0 and 1. DEFAULT: 0.25
#' @param bothsame    Set the visual amount of rounding to the same in
#'                    both x and y direction? If TRUE (the default),
#'                    the proportion relates to the shortest rectangle side.
#'                    This is visually correct only if \code{aspcorrect}
#'                    and \code{devcorrect} are both left at TRUE and 
#'                    \code{corfactor} is set correctly. bothsame DEFAULT: TRUE
#' @param aspcorrect  Correct for graph aspect ratio? DEFAULT: \code{bothsame}
#' @param devcorrect  Correct for device aspect ratio? DEFAULT: \code{bothsame}
#' @param corfactor   Aspect correction factor. I found this by trial and error.
#'                    More elegant solutions are welcome!
#'                    DEFAULT 1.3, works well for 7x5 (width x height) graphs
#' @param factorpoints Logical: plot points at inset locations to determine the
#'                    exact value for \code{corfactor} by measuring on screen.
#'                    DEFAULT: FALSE
#' @param corners     Vector with integers indicating which corners to round.
#'                    Starting bottom left, going clockwise. Zero to suppress
#'                    rounding. DEFAULT: 1:4
#' @param npoints     Total number of vertices for the corners. DEFAULT: 200
#' @param plot        Logical. Plot the polygon? FALSE to only compute coordinates.
#'                    DEFAULT: TRUE
#' @param \dots       Further arguments passed to \code{\link{polygon}},
#'                    like col, border, ...
#' 
roundedRect <- function(
xleft, ybottom, xright, ytop,
rounding=0.25,
bothsame=TRUE,
aspcorrect=bothsame,
devcorrect=bothsame,
corfactor=1.3,
factorpoints=FALSE,
corners=1:4,
npoints=200,
plot=TRUE,
...)
{
# abbreviated inputs and checks:
XL <- xleft
XR <- xright
YB <- ybottom
YT <- ytop
RR <- rounding
if(length(XL)>1) stop(   "xleft must be a single value, not ", length(XL))
if(length(XR)>1) stop(  "xright must be a single value, not ", length(XR))
if(length(YB)>1) stop( "ybottom must be a single value, not ", length(YB))
if(length(YT)>1) stop(    "ytop must be a single value, not ", length(YT))
if(length(RR)>1) stop("rounding must be a single value, not ", length(RR))
if(RR>1) warning("rounding recommended to be smaller than 1. It is ", RR)
if(RR<0) warning("rounding recommended to be larger than 0. It is ", RR)
if(!is.numeric(corners)) stop("corners must be ingtegers, not ", class(corners))
if(!all(corners %in% 0:4)) stop("corners must be (some of) the integers 0:4, not ", 
                                toString(corners))

XD <- XR-XL
YD <- YT-YB
if(bothsame) XD <- YD <- min(c(XD,YD))
xi <- RR*(XD) # x inset of rounded corner
yi <- RR*(YD)

asp <- diff(par("usr")[3:4])/diff(par("usr")[1:2]) # current aspect ratio y to x
dev <- dev.size()[1]/dev.size()[2]
if(aspcorrect) xi <- xi/asp
if(devcorrect) xi <- xi/dev
xi <- xi/corfactor

# elliptic corners function:
elx <- function(from,to) xi*cos(seq(from,to,length.out=npoints/4))
ely <- function(from,to) yi*sin(seq(from,to,length.out=npoints/4))

# x and y coordinates:
xc <- c(if(3 %in% corners) XR-xi+elx(0     ,pi/2  ) else XR, # corner 3 TR
        if(2 %in% corners) XL+xi+elx(pi/2  ,pi    ) else XL, # corner 2 TL
        if(1 %in% corners) XL+xi+elx(pi    ,3*pi/2) else XL, # corner 1 BL
        if(4 %in% corners) XR-xi+elx(3*pi/2,2*pi  ) else XR) # corner 4 BR
yc <- c(if(3 %in% corners) YT-yi+ely(0     ,pi/2  ) else YT, # corner 3 TR
        if(2 %in% corners) YT-yi+ely(pi/2  ,pi    ) else YT, # corner 2 TL
        if(1 %in% corners) YB+yi+ely(pi    ,3*pi/2) else YB, # corner 1 BL
        if(4 %in% corners) YB+yi+ely(3*pi/2,2*pi  ) else YB) # corner 4 BR
if(plot) polygon(x=xc, y=yc, ...)
if(factorpoints) points(c(XL+xi, XL+xi, XR-xi, XR-xi), 
                        c(YB+yi, YT-yi, YT-yi, YB+yi), pch=3, col=2, lwd=1)
invisible(data.frame(x=xc,y=yc))
}
