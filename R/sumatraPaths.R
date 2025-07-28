#' @title Get Sumatra PDF Viewer paths
#' @description 
#' Get Sumatra paths for \code{\link{sumatraInitialize}}. This will only work on windows.
#' @return paths
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2024
#' @seealso \code{\link{sumatraInitialize}}
#' @export
#' @examples
#' if(.Platform$OS.type == "windows") sumatraPaths()
#' @param open open the folders? DEFAULT: FALSE
#'
sumatraPaths <- function(open=FALSE)
{
if(.Platform$OS.type != "windows") stop("SumatraPDF is only available on Windows") 
# Exe path:
epath <- Sys.getenv("RSTUDIO_DESKTOP_EXE")
epath <- normalizePathCP(epath)
epath <- sub("rstudio.exe$", "resources/app/bin/sumatra", epath)
# Roampath
rpath <- Sys.getenv("APPDATA")
rpath <- normalizePathCP(rpath)
rpath <- paste0(rpath, "/SumatraPDF")
# open:
if(open) openFile(epath)
if(open) openFile(rpath)
# Output:
return(c(epath, rpath))
}
