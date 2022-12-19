#' @title title
#' @description description
#' @return ReturnValue
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2022
#' @export
#' @examples
#' if(requireNamespace("leaflet", quietly=TRUE) && 
#'    requireNamespace("leaflet.extras", quietly=TRUE))
#' bmap()
#'
#' @param a     Numerical vector.
#' @param plot  Logical. Should values be plotted? This can be turned off if
#'              only the computation results are needed. DEFAULT: TRUE
#' @param dummy currently_Unused
#' @param \dots Further arguments passed to \code{\link{plot}}
#'
bmap <- function(
x=13.12,
y=52.37,
zm=14,
...
)
{
if(!requireNamespace("leaflet", quietly=TRUE) ||
   !requireNamespace("leaflet.extras", quietly=TRUE))
return(warning("packages leaflet and leaflet.extras must be available for bmap"))
 
# create map, add controls:
rmap <- leaflet::leaflet()
rmap <- leaflet.extras::addSearchOSM(rmap, options=leaflet.extras::searchOptions(
        autoCollapse=TRUE, minLength=2, hideMarkerOnCollapse=TRUE, zoom=16))
rmap <- leaflet.extras::addControlGPS(rmap, options=leaflet.extras::gpsOptions(
        position="topleft", activate=TRUE, autoCenter=TRUE, maxZoom=16, setView=TRUE))
rmap <- leaflet::addMeasure(rmap, primaryLengthUnit="meters", primaryAreaUnit="hectares",
        activeColor="#3D535D", completedColor="#FF0000", position="topleft")
rmap <- leaflet::addScaleBar(rmap, position="topleft")
rmap <- leaflet.extras::addFullscreenControl(rmap)
rmap <- leaflet::setView(rmap, x, y, zoom=zm)
# add background map layer options, from mapview::mapviewGetOption("basemaps")
prov <- c(OSM="OpenStreetMap", Sat="Esri.WorldImagery", Topo="OpenTopoMap") 
for(pr in names(prov)) rmap <- leaflet::addProviderTiles(rmap, provider=unname(prov[pr]), 
        group=pr, options=leaflet::providerTileOptions(maxZoom=20))
rmap <- leaflet::addLayersControl(rmap, baseGroups=names(prov),
        overlayGroups=c("tracks","residential", "private", "large roads"),
        options=leaflet::layersControlOptions(collapsed=FALSE))
# Output:
return(rmap)
}
