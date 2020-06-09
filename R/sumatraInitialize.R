#' @title Set useful Sumatra PDF Viewer default settings
#' @description 
#' Set useful Sumatra PDF Viewer default settings. This will likely only work on windows.
#' At the given \code{path} with "SumatraPDF.exe", this creates two settings files.
#' Existing files are renamed ("_old_n" appended), not overwritten.\cr
#' Creates "sumatrapdfrestrict.ini" with \code{SavePreferences = 1} and \code{FullscreenAccess = 1}.\cr
#' Creates "SumatraPDF-settings.txt" with \code{ShowToc = false} and \code{DefaultDisplayMode = single page}.
#' \code{UiLanguage} gets filled in by Sumatra itself upon first opening.
#' @return path, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2020
#' @seealso \code{\link{openPDF}}\cr
#' \url{https://www.sumatrapdfreader.org/settings/settings.html}\cr
#' \url{https://github.com/sumatrapdfreader/sumatrapdf/blob/master/docs/sumatrapdfrestrict.ini}
#' @keywords file
#' @export
#' @examples
#' # sumatraInitialize() # only run in interactive mode
#'
#' @param path  Folder (not file) that contains "SumatraPDF.exe". 
#'              DEFAULT: extracted from \code{\link{Sys.getenv}("RSTUDIO_PANDOC")}, e.g.
#'              "C:/Program Files/RStudio/bin/sumatra"
#' @param roampath if not NULL, both files are also copied to this path, 
#'              e.g. C:/Users/berry/AppData/Roaming/SumatraPDF.
#'              DEFAULT: SumatraPDF folder at \code{\link{Sys.getenv}("APPDATA")}
#' @param openfolder Logical: Open folder after writing the files?
#'              Uses \code{\link{openFile}()}. DEFAULT: TRUE
#' 
sumatraInitialize <- function(
 path=sub("pandoc$", "sumatra", Sys.getenv("RSTUDIO_PANDOC")),
 roampath=paste0(Sys.getenv("APPDATA"),"/SumatraPDF"),
 openfolder=TRUE
 )
{
# Checks
if(.Platform$OS.type != "windows") stop("SumatraPDF is only available on Windows") 
if(!interactive()) stop("sumatraInitialize can only be used in an interactive session.")
checkFile(path)
dor <- !is.null(roampath)
if(dor) checkFile(roampath)
ok <- readline(paste0("Can I write files (no overwriting) at ", path, 
                      if(dor) paste(" and",roampath)," ? y/n: "))
if(!tolower(substr(ok,1,1))=="y") stop("You did not give write access.")

# Expand filenames
f1 <- file.path(path, "sumatrapdfrestrict.ini",  fsep="/")
f2 <- file.path(path, "SumatraPDF-settings.txt", fsep="/")

r1 <- file.path(roampath, "sumatrapdfrestrict.ini",  fsep="/")
r2 <- file.path(roampath, "SumatraPDF-settings.txt", fsep="/")

msg <- paste0("Created two SumatraPDF setting files.")

# Rename files if they exist
if(file.exists(f1))
  {
  fo1 <- newFilename(sub("restrict.ini$","restrict_old_1.ini",f1), quiet=TRUE)
  file.rename(f1, fo1)
  msg <- paste0(msg, "\nExisting file was renamed to ", fo1)
  }
if(file.exists(f2))
  {
  fo2 <- newFilename(sub("settings.txt$","settings_old_1.txt",f2), quiet=TRUE)
  file.rename(f2, fo2)
  msg <- paste0(msg, "\nExisting file was renamed to ", fo2)
  }
if(dor&&file.exists(r1))
  {
  ro1 <- newFilename(sub("restrict.ini$","restrict_old_1.ini",r1), quiet=TRUE)
  file.rename(r1, ro1)
  msg <- paste0(msg, "\nExisting file was renamed to ", ro1)
  }
if(dor&&file.exists(r2))
  {
  ro2 <- newFilename(sub("settings.txt$","settings_old_1.txt",r2), quiet=TRUE)
  file.rename(r2, ro2)
  msg <- paste0(msg, "\nExisting file was renamed to ", ro2)
  }

# Create new files
fn1 <- system.file("extdata/sumatrapdfrestrict.ini",  package="berryFunctions")
fn2 <- system.file("extdata/SumatraPDF-settings.txt", package="berryFunctions")
file.copy(fn1, f1)
file.copy(fn2, f2)

if(dor) file.copy(fn1, r1)
if(dor) file.copy(fn2, r2)

message(msg)
if(openfolder) 
 {
 openFile(path)
 if(dor) openFile(roampath)
 }

# Output:
return(invisible(path))
}
