#' @title runnning time conversion
#' @description display runnning times in useful units
#' @return list with time elements
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2020
#' @seealso \code{\link{runAxis}}
# @importFrom package fun1 fun2
#' @export
#' @examples
#' runTime(d=3.6, t="15:40")
#' runTime(d=3.6, t="15:10")
#' runTime(d=3.6, t="14:50")
#'
#' @param d  Numerical value: distance [km]
#' @param t  Charstring: time ["MM:SS"]
#'
runTime <- function(
d,
t
)
{
tsec <- strsplit(t, ":")[[1]]
tsec <- as.numeric(tsec)
tsec <- tsec[1]*60 + tsec[2]
pace <- tsec/d/60
pace <- paste0(floor(pace), ":", round0((pace-floor(pace))*60), " min/km")
speed <- paste(round(d/tsec*60*60,1), "km/h")
message(d, " km in ", t, ", ", pace, " = ", speed)
# Output:
return(invisible(list(distance=d, time=t, seconds=tsec, pace=pace, speed)))
}

