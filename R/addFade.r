
# Make existing colors fade away to white
# Berry Boessenkool, Feb 2016, berry-b@gmx.de

addFade <- function(
  col, # vector of color names (\code{\link{colors}}), hexadecimal or integer that can be interpreted by \code{\link{col2rgb}}
  fade=0.3, # Level of fading towards target. between 0 (target) and 1 (col). Can also be a vector.
  target="white", # target color that should be faded into.
  ... # Further arguments passed to \code{\link{colorRamp}}
  )
{
if(any(fade<0 | fade>1)) stop("fade must be between 0 and 1, not ",
                                 pastec(fade[fade<0|fade>1]))
cR <-  function(fade, col, target)
  {
  x <- colorRamp(c(col, target))(1-fade)   # , ...
  x2 <- if (ncol(x) == 4L)
             rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
        else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
  x2
  }
# output <- sapply(fade, FUN=cR)
output <- sapply(col, FUN=function(co) sapply(fade, FUN=cR, col=co, target=target))
return(output)
}
