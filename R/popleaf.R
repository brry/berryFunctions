#' create leaflet popup box info
#' 
#' combine data.frame columns into a leaflet popup-box compatible format
#' 
#' @return Vector with character strings
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr 2017
#' @seealso \code{\link{paste}}
#' @keywords aplot
#' @export
#' @examples
#' dat <- data.frame(a=14:16, b=letters[14:16], c=LETTERS[14:16],
#'                  lat=c(52.58,53.45,52.4), lon=c(6.34,7.23,13.05))
#' popleaf(dat)
#' dat$display <- popleaf(dat, 1:4)
#' 
#' \dontrun{ # Excluded from CRAN checks
#' library(leaflet)
#' leaflet(dat) %>% addTiles() %>% addCircleMarkers(~lon, ~lat, popup=~display)
#' }
#' 
#' @param df Data.frame
#' @param sel Columns to be selected (Names or index or TRUE/FALSE vector).
#'            DEFAULT: colnames(df)
#' @param exclude_geometry Remove column with the name "geometry" 
#'           (as in sf objects) from the display? DEFAULT: TRUE
#' @param na.rm Exclude NA entries from the display? DEFAULT: FALSE
#' 
popleaf <- function(
df,
sel=colnames(df),
exclude_geometry=TRUE,
na.rm=FALSE
)
{
df <- as.data.frame(df) # otherwise the next line doesn't work for sf
sel <- colnames(df[,sel, drop=FALSE])
if(exclude_geometry) sel <- sel[sel!="geometry"]
apply(df, MARGIN=1, function(x)
 {
 nna <- if(na.rm) !is.na(x[sel]) else TRUE
 sel <- sel[nna]
 paste(sel, ": ", x[sel], collapse="<br>")
 })
}
