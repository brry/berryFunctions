#' Animated spiral graph
#' 
#' Animation of (daily) time series along spiral
#' 
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{spiralDate}}, \code{\link{linLogHist}}
#' @keywords chron hplot aplot color
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @examples
#' 
#' set.seed(42)
#' x <- as.Date("1985-01-01")+0:5000
#' y <- cumsum(rnorm(5001))+50
#' y <- y + sin(0:5000/366*2*pi)*max(abs(y))/2
#' plot(x,y)
#' 
#' spiralDateAnim(x,y, steps=10, sleep=0.01) # 0.05 might be smoother...
#' spiralDateAnim(x,y, steps=20)
#' 
#' \dontrun{
#' ## Rcmd check --as-cran doesn't like to open external devices such as pdf,
#' ## so this example is excluded from running in the checks.
#' pdf("spiralDateAnimation.pdf")
#' spiralDateAnim(x,y, main="Example Transition", col=divPal(100), format=" ")
#' dev.off()
#' 
#' # if you have FFmpeg installed, you can use the animation package like this:
#' library2(animation)
#' saveVideo(spiralDateAnim(x,y, steps=300), video.name="spiral_anim.mp4", interval=0.1,
#'     ffmpeg="C:/Program Files/R/ffmpeg/bin/ffmpeg.exe")
#' 
#' }
#' 
#' @param dates,values,data Input as in \code{\link{spiralDate}}
#' @param steps Number of steps (images) in animation. DEFAULT: 100
#' @param sleep Pause time between frames, in seconds, passed to \code{\link{Sys.sleep}}. DEFAULT: 0
#' @param progbar Should a progress bar be drawn? Useful if you have a large dataset or many steps. DEFAULT: TRUE
#' @param \dots Further arguments passed to \code{\link{spiralDate}}
#' 
spiralDateAnim <- function(
dates,
values,
data,
steps=100,
sleep=0,
progbar=TRUE,
...
)
{
#
if(!missing(data)) # get vectors from data.frame
  {
  dates  <- getColumn(substitute(dates ), data)
  values <- getColumn(substitute(values), data)
  }
#
steps <- max(steps) # in case the user inputs a vector
s <- seq_len(steps) # basically, safe way to get  s <- 1:steps
# proportions of the dataset plotted in each step:
prop <- seq(0,1, len=steps+1)[-1]
# Progressbar, if wanted:
if(progbar) pb <- txtProgressBar(max=steps, style=3)
# actual plotting:
for(i in s){
spiralDate(dates=dates, values=values, prop=prop[i], ...)
if(sleep!=0) Sys.sleep(sleep)
  if(progbar) setTxtProgressBar(pb, i)
} # End of for-Loop
if(progbar) close(pb)
}
