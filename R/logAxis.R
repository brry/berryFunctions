# Function to label logarithmic axes
# berry-b@gmx.de, Sept 2014

logAxis <- function(
  side=1,      # Which \code{\link{axis}} are to be labeled?
  log=NULL,    # Is the axis logarithmic by plot(log="x")? internal DEFAULT: par("xlog") or "ylog"
  lcol="grey", # Color of gridlines drawn in the graph with \code{\link{abline}}, NA to suppress.
  lty=1, lwd=1,# Type of gridlines
  expr,        # Expression drawing over the ablines, like (points(x,y). Can be code within {braces}.
  las=1,       # LabelAxisStyle for the orientation of the labels
  from,             # Lower exponent OR vector with data, as in \code{\link{logVals}}
  to,               # High end exponent
  Range,            # Override from and to as range
  base,             # Bases to be used, eg. c(1,2,5) or 1
  big.mark="'",     # Symbol separating thousands, eg. space, comma, dot, etc. see "format" and "prettyNum"
  decimal.mark=".", # Character separating comma values, see "format" and "prettyNum"
  scientific=FALSE, # See \code{\link{format}}
  exponent=5,       # Starting at which exponent should \code{\link{logVals}} return an expression with exponents? DEFAULT: 5
  expobase1=FALSE,  # Should "n * " be appended before 10^exp if n=1?
  allbase=1:9,      # base for \code{$all} (for horizontal lines)
    ...)            # further arguments passed to axis, like \code{lwd, col.ticks, hadj, lty}, ...
{
for(side_i in side)
{ # loop around each side
if(side_i==1 | side_i==3) # vertical lines, labels at x-axis:
  {
  # set from and to:
  from_i <- if(missing(from)) par("usr")[1] else from
  to_i   <- if(missing(to))   par("usr")[2] else to
  # determine base:
  base_i <- if(missing(base)) {if(abs(to_i-from_i)>4) 1 else c(1,2,5)} else base
  # get labels and positions:
  lv <- logVals(from=from_i, to=to_i, Range=Range, base=base_i, big.mark=big.mark,
            decimal.mark=decimal.mark, scientific=scientific, exponent=exponent,
            expobase1=expobase1, allbase=allbase)
  # draw lines
  if(is.null(log)) log <- par("xlog")
  if(log) abline(v=lv$all,     col=lcol, lty=lty, lwd=lwd)
  else abline(v=log10(lv$all), col=lcol, lty=lty, lwd=lwd)
  }
else # horizontal lines, labels at y-axis:
  {
  # set from and to:
  from_i <- if(missing(from)) par("usr")[3] else from
  to_i   <- if(missing(to))   par("usr")[4] else to
  # determine base:
  base_i <- if(missing(base)) {if(abs(to_i-from_i)>4) 1 else c(1,2,5)} else base
  # get labels and positions:
  lv <- logVals(from=from_i, to=to_i, Range=Range, base=base_i, big.mark=big.mark,
            decimal.mark=decimal.mark, scientific=scientific, exponent=exponent,
            expobase1=expobase1, allbase=allbase)
  # draw lines
  if(is.null(log)) log <- par("ylog")
  if(log) abline(h=lv$all,     col=lcol, lty=lty, lwd=lwd)
  else abline(h=log10(lv$all), col=lcol, lty=lty, lwd=lwd)
  }
# axis labels:
if(log) axis(side=side_i, at=lv$vals,        labels=lv$labs, las=las, ...)
else    axis(side=side_i, at=log10(lv$vals), labels=lv$labs, las=las, ...)
} # End of loop
# Box to cover up the lines plotted over the existing box:
box("plot")
# overplot ablines with expr:
if(!missing(expr)) expr
# output:
return(invisible(lv))
} # End of function
