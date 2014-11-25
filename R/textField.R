
# write text in graphics with colored field underneath it for better readability
# Search Engine Keywords:
# R Text visible on top
# R labeling with color underneath
# R Creating text with a halo
# R Text with shadow

# Berry, April 2013 + March 2014, berry-b@gmx.de
# with inspiration taken from vegan:::ordilabel() and thanks to Jari Oksanen for his comments
# see also ade4:::s.label, which is simpler and doesn't work with logarithmic axes

textField <- function(
         x, # X coordinates, if necessary, they are recycled
         y, # Y coordinates
         labels=seq_along(x), # labels to be placed at the coordinates, as in \code\linktext}}
         fill="white",   # fill is recycled if necessary. With a message when quiet = FALSE
         border=NA,      # ditto for border
         expression=NA,  # If TRUE, labels are converted to expression for better field positioning through expression bounding boxes. If NA, it is set to TRUE for labels without line breaks (Newlines, "\n").
         margin=0.3,     # added field space around words (multiple of em/ex)
         field="rect",   # 'rectangle', 'ellipse', or 'rounded', partial matching is performed
         nv=1000,        # number of vertices for field = "ellipse" or "rounded". low: fast drawing. high: high resolution in vector graphics as pdf possible.
         rounding=0.75,  # between 0 and 1: portion of height that is cut off rounded at edges when field = "rounded"
         lty=par("lty"), # line type
         lwd=par("lwd"), # line width
         cex=par("cex"), # character expansion
         xpd=par("xpd"), # expand text outside of plot region ("figure")?
         adj=par("adj"), # vector of length one or two
         pos=NULL,       # in 'text', pos overrides adj values.
         offset=0.5,     # I want the field to still be drawn with adj, but have it based on pos
         quiet=FALSE,    # Suppress warning when Arguments are recycled?
         ...) # further arguments passed to strwidth and text, like font, vfont, family
{
# Partial matching field--------------------------------------------------------
PossibleValues <- c("rectangle", "ellipse", "rounded")
field <- PossibleValues[pmatch(field,  PossibleValues)]
#
# Recycling --------------------------------------------------------------------
# Recycle x or y, if one is shorter than the other. Code taken from xy.coords
nx <- length(x)  ;  ny <- length(y)
if( nx < ny )
  { x <- rep(x, length.out=ny)  ; nx <- length(x) }
  else
  y <- rep(y, length.out=nx)
if(length(labels) > nx) stop("more labels than coordinates were given.")
# Recycle arguments if necessary
if(! quiet)
 {
 rarg <- NULL # recycled arguments
 rtim <- NULL # number of times recycled
 if(length(labels)!=nx){rarg<-c(rarg,"labels"); rtim<-c(rtim,nx/length(labels))}
 if(length(fill)  !=nx){rarg<-c(rarg,"fill")  ; rtim<-c(rtim,nx/length(fill))}
 if(length(border)!=nx){rarg<-c(rarg,"border"); rtim<-c(rtim,nx/length(border))}
 if(length(lty)   !=nx){rarg<-c(rarg,"lty")   ; rtim<-c(rtim,nx/length(lty))}
 if(length(lwd)   !=nx){rarg<-c(rarg,"lwd")   ; rtim<-c(rtim,nx/length(lwd))}
 if(length(cex)   !=nx){rarg<-c(rarg,"cex")   ; rtim<-c(rtim,nx/length(cex))}
 if(length(rounding)!=nx & field=="rounded"){rarg<-c(rarg,"rounding");rtim<-c(rtim,nx/length(rounding))}
 if(!is.null(rarg))
  {
  rtim <- round(rtim, 2)
  warning("The following arguments have been recycled:\n",
          paste(format(rarg), format(rtim), "times", collapse="\n"))
  }                         # formatC(rtim, width=5)
 }# end if !quiet
if(length(labels) != nx ) labels <- rep(labels, length.out=nx)
if(length(fill)   != nx )   fill <- rep(  fill, length.out=nx)
if(length(border) != nx ) border <- rep(border, length.out=nx)
if(length(lty)    != nx )    lty <- rep(   lty, length.out=nx)
if(length(lwd)    != nx )    lwd <- rep(   lwd, length.out=nx)
if(length(cex)    != nx )    cex <- rep(   cex, length.out=nx)
if(length(rounding) != nx & field=="rounded") rounding <- rep(rounding, length.out=nx)
#
# Dimensioning -----------------------------------------------------------------
# better field positioning through expression bounding boxes:
 labels2 <- as.list(labels)
# labels without newline:
nl <- which(!sapply(labels2, grepl, pattern="\n"))
if(is.na(expression) & length(nl)>0 )
  for(i in nl) labels2[[i]] <- as.expression(labels2[[i]])
if(isTRUE(expression)) labels2 <- lapply(labels2, as.expression)
#
# Dimension of the character string in plot units:
w <- sapply(labels2, strwidth , cex=cex, ...)
h <- sapply(labels2, strheight, cex=cex, ...)
# Box height times number of line breaks
if(isTRUE(expression))
##   h <- h + h*sapply(gregexpr("\n", labels), function(x) sum(x>0)) # false
##labels=c("Bug","oo-\nbahg", "Bug-\nbahg\ngrh")
h <- sapply(as.list(labels), function(xx) 
    {
    xxsplit <- strsplit(xx, "\n")[[1]]
    sum(sapply( lapply(as.list(xxsplit), as.expression), strheight, cex=cex, ...))
    })
#browser() # this sometimes is a list!



#h <- strheight("lg", cex=cex, ...)
# Extra-space (margins) around characters:
if(field=="ellipse") margin <- margin + 1.5 # bigger margin for ellipses needed
if(is.na(margin[2])) margin[2] <- margin[1]
mar_x <- strwidth ("m", cex=cex, ...)*margin[1]  # 'em' = stringwidth of letter m
mar_y <- strheight("x", cex=cex, ...)*margin[2]  # in typography, analog for ex
# For logarithmic axes:
if(par("ylog")) y <- log10(y)
if(par("xlog")) x <- log10(x)
# Adjust the adj parameter for vertical positioning (if only one value is given):
if(is.na(adj[2]))
  adjy <- 0.5
  else
  adjy <- adj[2]
# Adjust adj parameter based on pos and offset:
# if(!is.null(pos))
#
# Drawing ----------------------------------------------------------------------
if(field=="rectangle")
# Plot rectangular fields: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{ xleft <- x-w*adj[1]-mar_x;  xright <- x-w*adj[1]+w+mar_x
ybottom <- y-h*adjy  -mar_y;    ytop <- y-h*adjy  +h+mar_y
rect(  xleft = if(par("xlog")) 10^xleft   else xleft,
      xright = if(par("xlog")) 10^xright  else xright,
     ybottom = if(par("ylog")) 10^ybottom else ybottom,
        ytop = if(par("ylog")) 10^ytop    else ytop,
        col=fill, border=border, xpd=xpd, lty=lty, lwd=lwd)
}else if(field=="ellipse")
#
# Plot elliptic fields: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
for(i in 1:nx)
  {xell <- x[i] + (w[i]+mar_x)/2*cos(seq(0, 2*pi, length=nv))
   yell <- y[i] + (h[i]+mar_y)/2*sin(seq(0, 2*pi, length=nv))
   polygon(x = if(par("xlog")) 10^xell else xell,
           y = if(par("ylog")) 10^yell else yell,
           col=fill[i], border=border[i], xpd=xpd, lty=lty[i], lwd=lwd[i])
  }
}
else if(field=="rounded")
#
# Plot rectangular fields with rounded corners: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
#if(par("ylog") | par("xlog")) stop("Rounded not yet possible with log axes")
if(any(rounding < 0 || rounding > 1  )) stop("Wrong rounding value. Needs to be between 0 and 1.")
asp <- diff(par("usr")[3:4])/diff(par("usr")[1:2]) # current aspect ratio y to x
for(i in 1:nx)
  {
  XL <- x[i] - w[i]*adj[1] - mar_x        # Left x position, as with rectangle
  XR <- x[i] - w[i]*adj[1] + w[i] + mar_x # right x
  YB <- y[i] - h[i]*adjy   - mar_y        # bottom y position
  YT <- y[i] - h[i]*adjy   + h[i] + mar_y # top y
  xi <- rounding[i]*(mar_x+h[i]/asp/2)  # x inset of rounded corner
  yi <- rounding[i]*(mar_y+h[i]/2)      # y inset
  elx <- function(from,to) xi*cos(seq(from,to,length.out=nv/4)) # elliptic corners function
  ely <- function(from,to) yi*sin(seq(from,to,length.out=nv/4))
  # x and y coordinates:
  xc <- c(XR-xi+elx(0,pi/2), XR-xi, XL+xi, XL+xi+elx(pi/2,pi), XL,    XL,
   XL+xi+elx(pi,3*pi/2), XL+xi, XR-xi, XR-xi+elx(3*pi/2,2*pi), XR,    XR)
  yc <- c(YT-yi+ely(0,pi/2), YT,    YT,    YT-yi+ely(pi/2,pi), YT-yi, YB+yi,
   YB+yi+ely(pi,3*pi/2), YB,    YB,    YB+yi+ely(3*pi/2,2*pi), YB+yi, YT-yi)
  polygon(x = if(par("xlog")) 10^xc else xc,
          y = if(par("ylog")) 10^yc else yc,
          col=fill[i], border=border[i], xpd=xpd, lty=lty[i], lwd=lwd[i])
  } # End of for loop
}
else stop("Wrong field type specified. Use 'rectangle', 'ellipse', or 'rounded'.")
#
# Writing ----------------------------------------------------------------------
# Write text:
text(if(par("xlog")) 10^x else x,  if(par("ylog")) 10^y else y,
     labels=labels, cex=cex, xpd=xpd, adj=adj, pos=pos, offset=offset, ...)
} # End of function

