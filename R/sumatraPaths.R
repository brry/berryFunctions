#' @title Get Sumatra PDF Viewer paths
#' @description 
#' Get Sumatra paths for \code{\link{sumatraInitialize}}. This will only work on windows.
#' @return paths
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2024
#' @seealso \code{\link{sumatraInitialize}}
#' @export
#' @examples
#' sumatraPaths()
#'
sumatraPaths <- function()
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
# Output:
return(c(epath, rpath))
}
