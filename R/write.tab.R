#' @title write table with different defaults
#' @description calls write.table with (personally) useful default values for the arguments.
#' if \code{open=TRUE}, tries to open the file in the default txt viewer.
#' @return full filename
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2021
#' @seealso \code{\link{write.tab}}
#' @keywords file
#' @importFrom utils write.table
#' @export
#' @examples
#' # Don't run on CRAN test machines:
#' \dontrun{
#' write.tab(iris)
#' write.tab(iris, "otherfile.txt")
#' write.tab(freeny)
#' write.tab(freeny, name_rn="Time")
#' unlink("iris.txt")
#' unlink("otherfile.txt")
#' unlink("freeny.txt")
#' }
#' @param x            Objekt to be written.
#' @param file         Filename. DEFAULT: NULL = [name of x].txt
#' @param quote        Write quatation marks around charstrings? DEFAULT: FALSE
#' @param sep          Column separator. DEFAULT: "\\t"
#' @param name_rn      If not NULL, this will be used as the name for a prepended
#'                     column with the rownames. DEFAULT: NULL
#' @param row.names    Should rownames be written in a pre-column that will
#'                     mess up alignment with column mnames? 
#'                     Use name_rn instead. DEFAULT: FALSE
#' @param fileEncoding Encoding of charstrings. DEFAULT: "UTF-8"
#' @param open         Try to open the output file? DEFAULT: TRUE
#' @param \dots        Further arguments passed to \code{\link{write.table}}
#'
write.tab <- function(
x,
file=NULL,
sep="\t",
name_rn=NULL,
row.names=FALSE,
quote=FALSE,
fileEncoding="UTF-8",
open=TRUE,
...
)
{
if(is.null(file)) file <- paste0(deparse(substitute(x)), ".txt")
if(!is.null(name_rn)) {x <- cbind(rownames(x), x) ; colnames(x)[1] <- name_rn}
write.table(x=x, file=file, sep=sep, row.names=row.names, 
            quote=quote, fileEncoding=fileEncoding, ...)
if(open) openFile(file)
return(normalizePath(file))
}
