\name{textField}
\alias{textField}
\title{write text to plot with halo underneath}
\description{write text to plot. A field the size of each label is drawn beneath it, so the text can be read easily even if there are many points in the plot. Fields can be rectangular, elliptic or rectangular with roundeed edges.}
\usage{textField(x, y, labels=seq_along(x), fill="white", border=NA, margin=0.3,
field="rect", nv=1000, rounding=0.4, lty=par("lty"), lwd=par("lwd"),
cex=par("cex"), xpd=par("xpd"), adj=par("adj"), pos=NULL, offset=0.5, 
quiet=FALSE, ...) }
\arguments{
  \item{x}{X coordinates, if necessary, they are recycled}
  \item{y}{Y coordinates}
  \item{labels}{labels to be placed at the coordinates, as in \code{\link{text}}. DEFAULT: seq_along(x)}
  \item{fill}{fill is recycled if necessary. With a message when quiet = FALSE. DEFAULT: "white"}
  \item{border}{ditto for border. DEFAULT: NA}
  \item{margin}{added field space around words (multiple of em/ex). DEFAULT: 0.3}
  \item{field}{'rectangle', 'ellipse', or 'rounded', partial matching is performed. DEFAULT: "rect"}
  \item{nv}{number of vertices for field = "ellipse" or "rounded". low: fast drawing. high: high resolution in vector graphics as pdf possible.. DEFAULT: 1000}
  \item{rounding}{between 0 and 1: portion of height that is cut off rounded at edges when field = "rounded". DEFAULT: 0.4}
  \item{lty}{line type. DEFAULT: par("lty")}
  \item{lwd}{line width. DEFAULT: par("lwd")}
  \item{cex}{character expansion. DEFAULT: par("cex")}
  \item{xpd}{expand text outside of plot region ("figure")?. DEFAULT: par("xpd")}
  \item{adj}{vector of length one or two. DEFAULT: par("adj")}
  \item{pos}{in 'text', pos overrides adj values.. DEFAULT: NULL}
  \item{offset}{I want the field to still be drawn with adj, but have it based on pos. DEFAULT: 0.5}
  \item{quiet}{Suppress warning when Arguments are recycled?. DEFAULT: FALSE}
  \item{\dots}{further arguments passed to strwidth and text, like font, vfont, family}
}
\details{specifying pos and offset will currently change the position of the text, but not of the field.\cr
srt is not supported yet.\cr
lend, ljoin and lmitre can not be specified for rect, to keep argument number low.\cr
density (crosshatch etc.) is not supported, as this would distract from the text.}
\value{None}
\references{with inspiration taken from \code{ordilabel} in package \code{vegan} and thanks to Jari Oksanen for his comments}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, April 2013 + March 2014}
\seealso{\code{\link{text}}. \code{s.label} in package \code{ade4}, which is not so versatile and doesn't work with logarithmic axes }
\examples{
# Text Fields with rectangles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(007); plot(rnorm(1e4)) ; abline(v=0:5*2e3, col=8)
# Default settings:
textField(5000, 0, "Here's some good text")
# right-adjusted text (the field box still extends 'margin' stringwidths em):
textField(2000, -1, "Some more (smores!)", cex=1.5, adj=0, col=2)
# Field color, no extra margin beyond baseline (excluding descenders):
textField(2000, -2, "more yet", col=2, fill="blue", margin=0)
# margin can be one number for both x and y direction ... :
textField(1000, 2, "Up we go", fill=7, margin=1.4)
# ... or two (x and y different), even negative:
textField(5000, 2, "to the right", col=2, fill=4, margin=c(-0.4, 0.9))
# Fonts can be set as well:
textField(5000, 1, "And boldly down in bold font", font=2, border=3)
# Text can expand outsinde of the plot region (figure) into the margins:
textField(11000, -2, "Hi, I'm a long block of text", adj=1, fill="red")
textField(11000, -3, "You're not outside the plot!", adj=1, xpd=TRUE, fill="red")
# And most parameters can be vectorized, while x/y are recycled:
textField(3000, c(-3, -3.7), c("0", "good"), border=c("red",3), lty=1:2)

# TextFields with other field shapes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(13);  plot(cumsum(rnorm(100)), type="l")
for(i in 2:7) lines(cumsum(rnorm(100)), col=i)

# Ellipsis (looks better in vector graphics like pdf):
textField(40, 4, "Here's some Text again, dude", field="ellipse", fill=3)
# Field type can be abbreviated (partial matching), margin needs adjustment:
textField(90, 5, "short", field="ell", fill=7, border=4, mar=-0.4)
textField(80, 2, "long and higher", field="ell", mar=c(0.5, 2.3), bor=4)

# Rectangular field with edges rounded:
textField(80,-2, "Rounded", field="rounded", fill="orange", border=4, cex=1.7)
# Rounded can also vectorized:
textField(30, c(2,0,-2,-4,-6), paste("rounding =",seq(0,1,len=5)), field="round",
    fill=(2:6), mar=1, rounding=seq(0,1,len=5), border=1)
# turn off warning about recycling:
textField(80, c(-5,-6.5), c("Ja", "Nein"), field="round", fill=6:8, quiet=TRUE)


# textField even works on logarithmic axes:
mylabel <- rev(c("This","is","the","logarithmic","Sparta"))
plot(10^runif(5000, -1,2), log="y", col=8)
textField(2500, c(0.1, 0.3, 4, 20, 100), mylabel, fill=2)
textField(1000, c(0.1, 0.3, 4, 20, 100), mylabel, fill=4, field="ell")
textField(4000, c(0.1, 0.3, 4, 20, 100), mylabel, fill=3, field="round", rounding=0.25)


# In most devices, vertical adjustment is slightly off when the character string
# contains no descenders. The default is for centered text:  adj = c(0.5, NA).
# For drawing the field, adj[2] is in this case set to 0.5.
# Text positioning is different for NA than for 0.5, see details of ?text
combis <- c("Ha","he","ro","ri","rj","Ye","Hg","hng","cg","Zx")
plot(1:10, t="n", xaxs="i", yaxs="i", xlim=c(0,11)) ; grid(nx=11, ny=9)
textField(1:10, 9, combis, fill=3, quiet=TRUE, cex=1.2, mar=0)
textField(1:10, 8, combis, fill=3, quiet=TRUE, cex=1.2, mar=0, adj=c(.5,.5))
textField(1:10, 7, combis, fill=3, quiet=TRUE, cex=1.2, mar=0, adj=c(0 ,0 ))
textField(1:10, 6, combis, fill=3, quiet=TRUE, cex=1.2, mar=0, adj=c(1 ,1 ))


# To Do: specify adj for the field position based on the values of pos and offset...
# offset is in fractions of character width, so cannot be linearly translated to adj.
plot(1:10, t="n"); grid()
text(2,2, "long label", pos=1, offset=0.5)
text(4,2, "long label", adj=c(0.5, 1.6))
text(6,2, "long label", pos=1, offset=0.5, cex=1.5)
text(8,2, "long label", adj=c(0.5, 1.6), cex=1.5)
text(2,4, "long label", pos=1, offset=0.6)
text(4,4, "long label", adj=c(0.5, 1.7))

}
\keyword{aplot}
