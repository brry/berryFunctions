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
#' 
popleaf <- function(
df,
sel=colnames(df)
)
{
sel <- colnames(df[,sel, drop=FALSE])
apply(df, MARGIN=1, function(x)
 {
 paste(sel, ": ", x[sel], collapse="<br>")
 })
}
