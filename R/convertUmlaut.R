#' Convert German Umlaute to ASCII
#' 
#' Convert German Umlaute (ae, oe, ue, ss) to ASCII.
#' Conversion happens case sensitive for the first three.

#' @return Character strings
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct-Nov 2016
#' @seealso \code{tools::\link[tools]{showNonASCII}}, \code{\link{gsub}},
#'         \code{\link{iconv}(x, to="ASCII//TRANSLIT")}
#' @keywords character
#' @export
#' @examples
#' \dontrun{
#' link <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/",
#'                "monthly/kl/recent/KL_Monatswerte_Beschreibung_Stationen.txt")
#' weatherstations <- read.fwf(link, widths=c(6,9,10,16,11,8,41,99), skip=3)
#' examples <- removeSpace(weatherstations[c(153, 509, 587, 2, 651, 851),7])
#' examples
#' convertUmlaut(examples) # note how lower and upper case is kept
#' }
#' 
#' @param x Character string(s) containing German Umlaute
#' 
convertUmlaut <- function(
x
)
{
x <- gsub("\U00E4", "ae", x)
x <- gsub("\U00F6", "oe", x)
x <- gsub("\U00FC", "ue", x)
x <- gsub("\U00DF", "ss", x)
x <- gsub("\U00C4", "Ae", x)
x <- gsub("\U00D6", "Oe", x)
x <- gsub("\U00DC", "Ue", x)
x
}
