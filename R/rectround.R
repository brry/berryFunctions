#' @title Rectangles with rounded corners
#' @description Draw \link{rect}angles with rounded corners via \code{\link{polygon}}
#' @return ReturnValue
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2017
#' @seealso \code{\link{textField}}
#' @keywords aplot
# @importFrom package fun1 fun2
#' @export
#' @examples
#' plot(1:10)  ;  rect(4,2,7,8, border=8)
#' rectround(4,2,7,8, rounding=0.1)
#' rectround(4,2,7,8, rounding=0.25) # default
#' rectround(4,2,7,8, rounding=0.5)
#' rectround(4,2,7,8, rounding=-0.1, border="red")
#' rectround(4,2,7,8, rounding=0.8, border="blue")
#' 
#' plot(1:10, asp=1)  ;  rect(4,2,7,8, border=8)
#' rectround(4,2,7,8)
#' rectround(4,2,7,8, aspcorrect=TRUE, border="red") # results depend on asp...
#' 
#' plot(1:10, asp=1.5)  ;  rect(4,2,7,8, border=8)
#' rectround(4,2,7,8)
#' rectround(4,2,7,8, aspcorrect=TRUE, border="red") # results depend on asp...
#' 
#' plot(1:10, asp=1)  ;  rect(4,2,7,8, border=8)
#' rectround(4,2,7,8) # difference only visible if rect is clearly not a square:
#' rectround(4,2,7,8, bothsame=TRUE, border="red")
#' rectround(4,2,7,8, bothsame=TRUE, aspcorrect=TRUE, border="blue")
#' 
#' @param xleft,ybottom,xright,ytop Single numbers with the outer end locations
#'                    of the rectangle.
#' @param rounding    Proportion of the box to round. Recommended to be between
#'                    0 and 0.5. DEFAULT: 0.25
#' @param aspcorrect  Correct for graph aspect ratio? DEFAULT: FALSE
#' @param bothsame    Set the visual amount of rounding to the same in
#'                    both x and y direction? If TRUE,
#'                    the proportion relates to the shortest rectangle side.
#'                    DEFAULT: FALSE
#' @param \dots       Further arguments passed to \code{\link{polygon}},
#'                    like col, border, ...
#' 
rectround <- function(
xleft, ybottom, xright, ytop,
rounding=0.25,
aspcorrect=FALSE,
bothsame=FALSE,
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

asp <- diff(par("usr")[3:4])/diff(par("usr")[1:2]) # current aspect ratio y to x

XD <- XR-XL
YD <- YT-YB
if(bothsame) XD <- YD <- min(c(XD,YD))

xi <- RR*(XD) # x inset of rounded corner
yi <- RR*(YD)
if(aspcorrect) xi <- xi/asp
nv <- 1000 # number of vertices

# elliptic corners function:
elx <- function(from,to) xi*cos(seq(from,to,length.out=nv/4))
ely <- function(from,to) yi*sin(seq(from,to,length.out=nv/4))

# x and y coordinates:
xc <- c(XR-xi+elx(0,pi/2), XR-xi, XL+xi, XL+xi+elx(pi/2,pi), XL,    XL,
        XL+xi+elx(pi,3*pi/2), XL+xi, XR-xi, XR-xi+elx(3*pi/2,2*pi), XR,    XR)
yc <- c(YT-yi+ely(0,pi/2), YT,    YT,    YT-yi+ely(pi/2,pi), YT-yi, YB+yi,
        YB+yi+ely(pi,3*pi/2), YB,    YB,    YB+yi+ely(3*pi/2,2*pi), YB+yi, YT-yi)
polygon(x=xc, y=yc, ...)
}
