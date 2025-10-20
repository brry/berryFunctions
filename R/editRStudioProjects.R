#' @title edit recently opened Rprojects
#' @description edit the list of recently opened Rprojects in Rstudio
#' @return the path of the two modified files (invisibly)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2025
#' @keywords file
#' @export
#' @examples
#' editRStudioProjects()
#'
#' @param path currently ignored
#'
editRStudioProjects <- function(path=NULL)
{
path <- "~/.local/share/rstudio"
if(Sys.info()["sysname"] == "Windows") 
        path <- paste0(Sys.getenv("LOCALAPPDATA"),"/RStudio")
path <- normalizePathCP(paste0(path,"/monitored/lists"))
f1 <- paste0(path,"/project_mru")
f2 <- paste0(path,"/project_name_mru")
checkFile(c(f1,f2))
p1 <- readLines(f1)
p2 <- readLines(f2)
del <- select.list(p2, multiple=TRUE, title="Which projects should be removed?", graphics=TRUE)
del <- match(del, p2)
writeLines(p1[-del], f1) ; writeLines(p2[-del], f2)
return(invisible(path))
}
